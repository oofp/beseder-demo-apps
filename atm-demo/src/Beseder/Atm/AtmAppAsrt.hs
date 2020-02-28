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
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLabels       #-}
{-# LANGUAGE AllowAmbiguousTypes    #-}

{-# OPTIONS_GHC -fomit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Beseder.Atm.AtmAppAsrt where

import           Protolude                    hiding (Product, handle, return, gets, lift, liftIO,
                                              (>>), (>>=), forever, until,try,on, wait)
import           Beseder.Base.ControlData                                               
import           Data.String 

import           Beseder.Base.Common                                               
import           Beseder.Atm.Resources.CashDispenserRes
import           Beseder.Atm.Resources.AccountRes
import qualified Beseder.Atm.Resources.AccountRes as AccountRes
import           Beseder.Atm.Resources.CardReaderRes
import           Beseder.Atm.Resources.TerminalRes
import           Beseder.Resources.Timer
--import           Beseder.Atm.Resources.Types.Domain                                               
import           GHC.Exts (Any)    

type IdleState m resDsp resCard resTerm = 
  '[( StDispenserIdle m resDsp "dsp", 
    ( StCardReaderIdle m resCard "card", StTerminalIdle m resTerm "term"))
   ]

type IsIdleState = 
  "dsp"  :? IsDispenserIdle :&&
  "card" :? IsCardReaderIdle :&&
  "term" :? IsTerminalIdle    

dispenseTimeout :: Int
dispenseTimeout = 30

-- tradeoff between easy to refactor and conciesness 
wait = nextEv

ensuring :: forall sp1 sp m f_sub. STransData m (sp :&& sp1) f_sub () -> STransData m sp (EmbedFunc sp1 f_sub) ()
ensuring = try

repeatBreakingOn :: forall sp1 sp m f_sub. STransData m (sp :&& (Not sp1)) f_sub () -> STransData m sp  (EmbedFunc (Not sp1) (ForeverFunc f_sub)) ()
repeatBreakingOn hnd = try @(Not sp1) $ forever hnd

waitFor = skipTo
 
atmAppAsrtData :: forall resCard resAcc resTerm m.
  ( CardReader m resCard
  , Account m resAcc
  , Terminal m resTerm
  ) => AccountRes.ResPar m resAcc -> STransData m NoSplitter _ ()
atmAppAsrtData accResPar = do
  assert @IsIdleState
  wait >> assert @(Not ("card" :? IsCardReaderIdle))
  ensuring @("card" :? IsCardInserted) $ do
    newRes #acc accResPar
    handleAuthentication @resCard @resTerm
    ensuring @(IsUserLoggedIn "acc") $ do 
      handleUserSession @resAcc @resTerm
  handleCleanup 
  assert @IsIdleState   
  -- label #finishing  

handleAuthentication :: forall resCard resTerm m sp.
  ( CardReader m resCard
  , Terminal m resTerm
  ) => STransData m sp _ () --IsActiveUser _ ()
handleAuthentication = do --repeatUnless/ repeatWhile
  assert @("acc" :? IsSessionIdle :&& "term" :? IsTerminalIdle)
  invoke #term GetPasscode
  repeatBreakingOn @("acc" :? IsAccountBlocked 
                      :|| "acc" :? IsUserAuthenticated 
                      :|| "term" :? IsPasscodeCancelled) $ do
    wait >> assert @("term" :? IsPasscodeProvided) -- if cancel would break
    pcode <- opRes #term (passcode @resTerm)
    cardInfo <- opRes #card (cardDetails @resCard)
    invoke #acc (Authenticate cardInfo pcode) >> wait >>
      assert @("acc" :? IsAuthenticationFailed) -- if blocked or successfully authenticated would break
    invoke #acc AckAuthFailure
    invoke #term ShowNoticeWrongPasscode >> wait 
    assert @("term" :? IsGettingPasscode)

handleUserSession :: forall resAcc resTerm m sp.
  ( Account m resAcc
  , Terminal m resTerm
  ) => STransData m sp _ () --IsActiveUser _ ()
handleUserSession = do
  invoke #term SelectService
  forever $ do
    assert @("term" :? IsSelectingService) >> wait
    caseOf $ do
      on @("term" :? IsBalanceSelected) (handleBalance @resAcc)
      on @("term" :? IsWithdrawalSelected) (handleWithdrawal @resTerm)
      on @("term" :? IsQuitSelected) $ do
        invoke #term (ShowNoticeEjectingCard)
        invoke #card EjectCard -- it will break the loop (due to ensuring IsCardInserted)
      endCase  
  
type IsActiveUser = NoSplitter :&& "card" :? IsCardInserted :&& (IsUserLoggedIn "acc") 

handleBalance :: forall resAcc m sp. Account m resAcc => STransData m sp _ () --IsActiveUser _ ()
handleBalance = do
  assert @("term" :? IsBalanceSelected :&& "acc" :? IsUserAuthenticated)
  invoke #acc QueryBalance >> wait
  caseOf $ do
    on @("acc" :? IsBalanceAvailable) $ do 
      blnc <- opRes #acc (accountBalance @resAcc)
      invoke #term (ShowBalance blnc)
      invoke #acc AckBalance
      nextEv
    on @("term" :? IsRequestCancelled) 
      handleCancelReq 
    endCase
  assert @("term" :? IsSelectingService :&& "acc" :? IsUserAuthenticated)

handleWithdrawal :: forall resTerm m sp. (Terminal m resTerm) => STransData m sp _ () 
handleWithdrawal = do
  assert @("term" :? IsWithdrawalSelected :&& "acc" :? IsUserAuthenticated)
  amnt <- opRes #term (withdrawalAmount @resTerm)
  invoke #acc (ReserveFunds amnt) >> wait
  caseOf $ do
    on @("acc" :? IsFundsReserved) $ do
      -- scopeRes $ do -- scope dispTimer
      invoke #term (ShowWithdrawInstruction amnt)
      invoke #dsp (DispenseCash amnt)
      newRes #dispTimer TimerRes
      invoke #dispTimer (StartTimer dispenseTimeout)
      wait 
      on @("dispTimer" :? IsTimerTriggered) $ do
        invoke #dsp RollbackCash
        nextEv
      clear #dispTimer  
      caseOf $ do
        on @("dsp" :? IsCashCollected) $ do 
          invoke #dsp AckCollected
          invoke #acc ConfirmWithdrawal
          invoke #term SelectService
          -- label #dispensingCollected
        on @("dsp" :? IsRolledBack) $ do  
          invoke #dsp AckRolledBack
          invoke #acc RollbackWithdrawal
          invoke #term (ShowNoticeAndQuit "Withdrawal timeout. Keeping your card")
          invoke #card EatCard -- break here due to ensuring CardInserted
        endCase  
    on @("term" :? IsRequestCancelled) $ do
      handleCancelReq
      -- label #withdrawCancelled
    on @("acc" :? IsFundsReservationFailed) $ do
      invoke #term (ShowNoticeAndSelect "Cannot withdraw this amount")
      invoke #acc AckReserveFailure
      nextEv
    endCase  
  assert @("term" :? IsSelectingService :&& "acc" :? IsUserAuthenticated)

handleCancelReq :: STransData m sp _ ()    
handleCancelReq = do
  assert @("term" :? IsRequestCancelled)
  invoke #acc CancelReq
  invoke #term AckRequestCancellation
  assert @("term" :? IsSelectingService :&& "acc" :? IsUserAuthenticated)

handleCleanup :: STransData m sp _ ()      
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
    label #handleCleanup  
  pumpEvents 
  on @("acc" :? IsSessionIdle) $ do
    clear #acc
  label #aboutToReleaseTerminal  
  invoke #term ReleaseTerminal
  pumpEvents
  invoke #card EnableCardReader  
  assert @IsIdleState

mkSTransDataTypeAny "atmAppAsrtData" "ATMAsrtFunc"   

type ATMRes = Eval (ATMAsrtFunc NoSplitter (IdleState IO () () ()))
type ATMValid = ValidateSteps '[] ATMAsrtFunc NoSplitter (IdleState IO () () ())
type ATMDiagram = StateDiagramSym ATMAsrtFunc (IdleState IO () () ())

-- :kind!  ValidateSteps '[] ATMAsrtFunc NoSplitter (IdleState IO () () ())
-- :kind!  ValidateSteps '["aboutToReleaseTerminal", "handleCleanup"] ATMFuncAny NoSplitter (IdleState IO () () ())
--
  
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

