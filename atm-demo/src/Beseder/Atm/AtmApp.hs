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

atmAppData :: (_) => Beseder.Atm.Resources.AccountRes.ResPar m accRes -> STransData m NoSplitter _ ()
atmAppData accResPar = do
  nextEv -- wait for card inserted (the only possible event)
  try @("card" :? IsCardInserted) $ do
    newRes #acc accResPar
    handleAuthentication
    label #authCompleted
    try @(IsUserLoggedIn "acc") $ do 
      handleLoggedInUser
  label #finishing  

handleAuthentication :: (_) => STransData m sp _ () --IsActiveUser _ ()
handleAuthentication = do 
  try @(Not ("acc" :? IsAccountBlocked 
        :|| "acc" :? IsUserAuthenticated 
        :|| "term" :? IsPasscodeCancelled)) $ do
    invoke #term GetPasscode
    forever $ do
      nextEv -- term: GettingPasscode -> [PasscodeProvided, PasscodeCancelled]
      pcode <- opRes #term passcode
      cardInfo <- opRes #card cardDetails 
      invoke #acc (Authenticate cardInfo pcode)
      nextEv -- acc: Authenticating -> [ UserAuthenticated m res, AuthenticationFailed m res, AccountBlocked m res]
      invoke #term ShowNoticeWrongPasscode
      invoke #acc AckAuthFailure
      nextEv -- term: ShowingNoticeWrongPasscode -> GettingPasscode
      label #handleAuthentication

handleLoggedInUser :: (_) => STransData m sp _ () --IsActiveUser _ ()
handleLoggedInUser = do
  invoke #term SelectService
  label #selectingService
  forever $ do
    label #selectLoopStart  
    nextEv
    onOrElse @("term" :? IsBalanceSelected)
      handleBalance
      ( onOrElse @("term" :? IsWithdrawalSelected) 
          handleWithdrawal
          ( do -- add condition for clarity
              invoke #term (ShowNoticeEjectingCard)
              invoke #card EjectCard
          )
      ) 
    label #selectLoopCompleted  
  
type IsActiveUser = NoSplitter :&& "card" :? IsCardInserted :&& (IsUserLoggedIn "acc") 

handleBalance :: (_) => STransData m sp _ () --IsActiveUser _ ()
handleBalance = do
  -- label #balanceEntered
  invoke #acc QueryBalance 
  nextEv
  --label #balanceNext
  onOrElse @("acc" :? IsBalanceAvailable) -- not cancelled 
    ( do
        blnc <- opRes #acc accountBalance
        invoke #acc AckBalance
        invoke #term (ShowBalance blnc)
        nextEv
    )
    (on @("term" :? IsRequestCancelled) $ do
      handleCancelReq 
    )
  label #balanceCompleted

handleWithdrawal :: (_) => STransData m sp _ () -- IsActiveUser _ ()
handleWithdrawal = do
  label #withdrawalEntered
  amnt <- opRes #term withdrawalAmount
  invoke #acc (ReserveFunds amnt)
  label #withdrawal
  nextEv
  label #withdrawalNext
  onOrElse @("acc" :? IsFundsReserved) 
    ( do 
        invoke #term (ShowWithdrawInstruction amnt)
        invoke #dsp (DispenseCash amnt)
        newRes #dispTimer TimerRes
        invoke #dispTimer (StartTimer dispenseTimeout)
        nextEv
        on @("dispTimer" :? IsTimerTriggered) $ do
          invoke #dsp RollbackCash
          nextEv
          -- label #dispensingRollback
        clear #dispTimer  
        onOrElse @("dsp" :? IsCashCollected) 
          (do 
            invoke #dsp AckCollected
            invoke #acc ConfirmWithdrawal
            invoke #term SelectService
            label #dispensingCollected
          )
          (do 
            invoke #dsp AckRolledBack
            invoke #acc RollbackWithdrawal
            invoke #term (ShowNoticeAndQuit "Withdrawal timeout. Keeping your card")
            invoke #card EatCard 
            label #dispensingTimeout -- will be empty as we have condition on CardInserted
          )
    )
    ( onOrElse @("term" :? IsRequestCancelled)
        ( do
            handleCancelReq
            label #withdrawCancelled
        )
        ( on @("acc" :? IsFundsReservationFailed) $ do
            invoke #acc AckReserveFailure
            invoke #term (ShowNoticeAndSelect "Cannot withdraw this amount")
            nextEv
            label #fundsReservationFailed
        )
    )

handleCancelReq :: (_) => STransData m sp _ ()    
handleCancelReq = do
  invoke #acc CancelReq
  invoke #term AckRequestCancellation

{-
                                               :|| ("acc" :? IsAuthenticationFailed))))
                                   '[(St (DispenserIdle IO ()) "dsp",
                                      (St (CardInserted IO ()) "card",
                                       (St (SelectingService IO ()) "term",
                                        St (ReservedingFunds m accRes) "acc"))),
                                     (St (DispenserIdle IO ()) "dsp",
                                      (St (CardInserted IO ()) "card",
                                       (St (WithdrawalSelected IO ()) "term",
                                        St (FundsReservationFailed m accRes) "acc")))]]]]]]]]
-}

-- :t evalSTransData' (atmAppData undefined) (Proxy @(IdleState IO () () ()))

-- :t evalSTransDataApp' (atmAppData undefined) (Proxy @(IdleState IO () () ()))

-- :t evalSTransDataLabels' (atmAppData undefined) (Proxy @(IdleState IO () () ()))

-- :t evalSTransDataNamedLabels' #balanceEntered (atmAppData undefined) (Proxy @(IdleState IO () () ()))