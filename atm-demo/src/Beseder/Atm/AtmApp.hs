{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PartialTypeSignatures  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE RebindableSyntax       #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLabels #-}

{-# OPTIONS_GHC -fomit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Beseder.Atm.AtmApp where

import           Protolude                    hiding (Product, handle, return, gets, lift, liftIO,
                                              (>>), (>>=), forever, until,try,on)
import           Beseder.Base.ControlData                                               
import           Beseder.Base.Common                                               
import           Beseder.Misc.Misc  
import           Control.Monad.Cont (ContT)                                            
import           Data.String 
    
import           Beseder.Atm.Resources.CashDispenserRes
import           Beseder.Atm.Resources.AccountRes
import           Beseder.Atm.Resources.CardReaderRes
import           Beseder.Atm.Resources.TerminalRes
import           Beseder.Resources.Timer

type IdleState m resDsp resCard resTerm = 
  '[( StDispenserIdle m resDsp "dsp", 
    ( StCardReaderIdle m resCard "card", StTerminalIdle m resTerm "term"))
   ]

dispenseTimeout :: Int
dispenseTimeout = 30

--atmAppData :: forall m accRes termRes cardRes. 
--  ( Account m accRes, Terminal m termRes, CardReader m cardRes, _
--  ) => Beseder.Atm.Resources.AccountRes.ResPar m accRes -> STransData m NoSplitter _ ()
atmAppData :: (_) => Beseder.Atm.Resources.AccountRes.ResPar m accRes -> STransData m NoSplitter _ ()
atmAppData accResPar = do
  nextEv -- wait for card inserted (the only possible event)
  try @("card" :? IsCardInserted) $ do
    newRes #acc accResPar
    handleAuthentication
    -- label #authCompleted
    try @(IsUserLoggedIn "acc") $ do 
      handleLoggedInUser
  handleCleanup    
  -- label #finishing  

handleAuthentication :: (_) => STransData m sp _ () --IsActiveUser _ ()
handleAuthentication = do 
  try @(Not ("acc" :? IsAccountBlocked 
        :|| "acc" :? IsUserAuthenticated 
        :|| "term" :? IsPasscodeCancelled)) $ do
    invoke #term GetPasscode
    forever $ do
      nextEv -- term: GettingPasscode -> [PasscodeProvided, PasscodeCancelled]
      assert @("term" :? IsPasscodeProvided)
      pcode <- opRes #term passcode
      cardInfo <- opRes #card cardDetails 
      invoke #acc (Authenticate cardInfo pcode)
      nextEv -- acc: Authenticating -> [ UserAuthenticated m res, AuthenticationFailed m res, AccountBlocked m res]
      invoke #term ShowNoticeWrongPasscode
      invoke #acc AckAuthFailure
      nextEv -- term: ShowingNoticeWrongPasscode -> GettingPasscode
      -- label #handleAuthentication

handleLoggedInUser :: (_) => STransData m sp _ () --IsActiveUser _ ()
handleLoggedInUser = do
  invoke #term SelectService
  -- label #selectingService
  forever $ do
    -- label #selectLoopStart  
    nextEv
    caseOf $ do
      on @("term" :? IsBalanceSelected) handleBalance
      on @("term" :? IsWithdrawalSelected) handleWithdrawal
      on @("term" :? IsQuitSelected) $ do
        invoke #term (ShowNoticeEjectingCard)
        invoke #card EjectCard
      endCase  
    -- label #selectLoopCompleted  
  
type IsActiveUser = NoSplitter :&& "card" :? IsCardInserted :&& (IsUserLoggedIn "acc") 

handleBalance :: (_) => STransData m sp _ () --IsActiveUser _ ()
handleBalance = do
  -- label #balanceEntered
  invoke #acc QueryBalance 
  nextEv
  caseOf $ do
    on @("acc" :? IsBalanceAvailable) $ do -- not cancelled 
      blnc <- opRes #acc accountBalance
      invoke #term (ShowBalance blnc)
      invoke #acc AckBalance
      nextEv
    on @("term" :? IsRequestCancelled) 
      handleCancelReq 
    endCase
  -- label #balanceCompleted

handleWithdrawal :: (_) => STransData m sp _ () -- IsActiveUser _ ()
handleWithdrawal = do
  -- label #withdrawalEntered
  amnt <- opRes #term withdrawalAmount
  invoke #acc (ReserveFunds amnt)
  -- label #withdrawal
  nextEv
  -- label #withdrawalNext
  caseOf $ do
    on @("acc" :? IsFundsReserved) $ do
      block $ do
        invoke #term (ShowWithdrawInstruction amnt)
        invoke #dsp (DispenseCash amnt)
        newRes #dispTimer TimerRes
        invoke #dispTimer (StartTimer dispenseTimeout)
      nextEv
      on @("dispTimer" :? IsTimerTriggered) $ do
        invoke #dsp RollbackCash
        nextEv
        -- -- label #dispensingRollback
      clear #dispTimer  
      caseOf $ do
        on @("dsp" :? IsCashCollected) $ block $ do 
          invoke #dsp AckCollected
          invoke #acc ConfirmWithdrawal
          invoke #term SelectService
          -- label #dispensingCollected
        on @("dsp" :? IsRolledBack) $ block $ do  
          invoke #dsp AckRolledBack
          invoke #acc RollbackWithdrawal
          invoke #term (ShowNoticeAndQuit "Withdrawal timeout. Keeping your card")
          invoke #card EatCard 
          -- label #dispensingTimeout -- will be empty as we have condition on CardInserted
        endCase  
    on @("term" :? IsRequestCancelled) $ do
      handleCancelReq
      -- label #withdrawCancelled
    on @("acc" :? IsFundsReservationFailed) $ do
      invoke #acc AckReserveFailure
      invoke #term (ShowNoticeAndSelect "Cannot withdraw this amount")
      nextEv
      -- label #fundsReservationFailed
    endCase  

handleCancelReq :: (_) => STransData m sp _ ()    
handleCancelReq = do
  invoke #acc CancelReq
  invoke #term AckRequestCancellation

handleCleanup :: (_) => STransData m sp _ ()      
handleCleanup = do
  -- label #enteredCleanup  
  on @("acc" :? IsUserAuthenticated) $ do
    invoke #acc Logout
  on @("term" :? IsPasscodeCancelled :&& "card" :? IsCardInserted) $ do
    invoke #term ShowNoticeEjectingCard 
    invoke #card EjectCard
  on @("acc" :? IsAccountBlocked :&& "card" :? IsCardInserted) $ do
    invoke #card EatCard
    invoke #term ShowAccountBlockedNotice
    invoke #acc AckAccountBlocked
  on @("card" :? IsCardInvalid) $ do
    invoke #card AckInvalidCard
    invoke #term ShowInvalidCardNotice 
  -- label #handleCleanup  
  pumpEvents 
  on @("acc" :? IsSessionIdle) $ do
    clear #acc
  --label #aboutToReleaseTerminal  
  invoke #term ReleaseTerminal
  pumpEvents
  invoke #card EnableCardReader  

--evalData :: (_) => forall resDsp resCard resTerm. Proxy _
--evalData = evalSTransDataLabels' (atmAppData undefined) (Proxy @(IdleState TaskQ resDsp resCard resTerm))

-- evalAtmAppData :: (_) => (STransData m sp f a -> Proxy xs -> Proxy ys) -> Proxy ys
--evalAtmAppData evalFunc = evalFunc (atmAppData undefined) (Proxy @(IdleState IO () () ())) 

-- :t evalSTransData' (atmAppData undefined) (Proxy @(IdleState IO () () ()))

-- :t evalSTransDataApp' (atmAppData undefined) (Proxy @(IdleState IO () () ()))

-- :t evalSTransDataLabels' (atmAppData undefined) (Proxy @(IdleState IO () () ()))

-- :t evalSTransDataNamedLabels' #handleCleanup (atmAppData undefined) (Proxy @(IdleState IO () () ()))
-- :t getLabel' #handleCleanup (atmAppData undefined) (Proxy @(IdleState IO () () ()))

-- :t getSTransDiagramSymbol' (atmAppData undefined) (Proxy @(IdleState IO () () ()))
-- :t getSTransDiagramStates' (atmAppData undefined) (Proxy @(IdleState IO () () ()))

-- :t getLabel' #finishing (atmAppData undefined) (Proxy @(IdleState IO () () ()))
-- :t validateSTransData' (atmAppData undefined) (Proxy @(IdleState IO () () ()))
-- :t getError' (atmAppData undefined) (Proxy @(IdleState IO () () ()))
-- :t validateSteps' (atmAppData undefined) (Proxy @'["aboutToReleaseTerminal"]) (Proxy @(IdleState IO () () ()))

--intr :: (_) => STrans (ContT Bool) TaskQ NoSplitter (IdleState TaskQ resDsp resCard resTerm) _ _ _ ()  
--intr :: (_) => STrans (ContT Bool) TaskQ NoSplitter (IdleState TaskQ resDsp resCard resTerm) (IdleState TaskQ resDsp resCard resTerm) _ _ ()  
--intr = interpret  (atmAppData undefined)  --  undefined --

