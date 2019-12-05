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


module Beseder.Atm.AtmApp where

import           Protolude                    hiding (Product, handle, return, gets, lift, liftIO,
                                              (>>), (>>=), forever, until,try,on)
import           Beseder.Base.ControlData                                               
import           Beseder.Base.Common
import           Beseder.Misc.Misc
import           Beseder.Resources.Timer
import           Data.String 
    
import           Beseder.Atm.Resources.CashDispenserRes
import           Beseder.Atm.Resources.AccountRes
import           Beseder.Atm.Resources.CardReaderRes
import           Beseder.Atm.Resources.TerminalRes
        

type IdleState m resDsp resCard resTerm = 
  '[( StDispenserIdle m resDsp "dsp", 
    ( StCardReaderIdle m resCard "card", StTerminalIdle m resTerm "term"))
   ]

dispenseTimeout :: Int
dispenseTimeout = 30

atmAppData :: (_) => Beseder.Atm.Resources.AccountRes.ResPar m accRes -> STransData m NoSplitter _ ()
atmAppData accResPar = do
  --skipTo @("card" :? IsCardInserted) 
  nextEv
  try @("card" :? IsCardInserted) $ do
    newRes #acc accResPar
    invoke #term GetPasscode
    nextEv
    on @("term" :? IsPasscodeProvided) $ do
      pcode <- opRes #term passcode
      cardInfo <- opRes #card cardDetails 
      invoke #acc (Authenticate cardInfo pcode)
      nextEv
      try @(IsUserLoggedIn "acc") $ do
        invoke #term SelectService
        nextEv
        onOrElse @("term" :? IsBalanceSelected)
          handleBalance
          (on @("term" :? IsWithdrawalSelected) handleWithdrawal) 
  

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
    (invoke #acc CancelReq)    
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
            label #dispensingTimeout -- will be empty
          )
    )
    (label #fundsReserveFailure)
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