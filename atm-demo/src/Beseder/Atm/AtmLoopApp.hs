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

module Beseder.Atm.AtmLoopApp where

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
--import           Beseder.Resources.Timer.PaceRes

type IdleState m resDsp resCard resTerm resPace resAcc = 
  '[( StDispenserIdle m resDsp "dsp", 
    ( StCardReaderIdle m resCard "card", 
    ( StTerminalIdle m resTerm "term", 
    ( StSessionIdle m resAcc "acc", StPaceActive m resPace "pace"))))
   ]

dispenseTimeout :: Int
dispenseTimeout = 30

atmAppLoopData :: (_) => STransData m NoSplitter _ ()
atmAppLoopData = do
  handleEvents $ do
    label #allStates
    caseOf $ do
      on @("card" :? IsCardInvalid) $ do
        invoke #card AckInvalidCard
        invoke #term ShowInvalidCardNotice 
      on @("card" :? IsCardInserted) $ do
        caseOf $ do 
          on @("term" :? IsTerminalIdle) $ do
            invoke #term GetPasscode
          on @("term" :? IsGettingPasscode) $ do -- continue
            label #gettingPasscode
          on @("term" :? IsPasscodeCancelled) $ do 
            invoke #term ShowNoticeEjectingCard 
            invoke #card EjectCard
          on @("term" :? IsPasscodeProvided :&& "acc" :? IsSessionIdle) $ do
            pcode <- opRes #term passcode
            cardInfo <- opRes #card cardDetails 
            invoke #acc (Authenticate cardInfo pcode)
          on @("acc" :? IsAuthenticating) $ do
            label #authenticating -- continue
          on @("acc" :? IsAccountBlocked) $ do
            invoke #term ShowAccountBlockedNotice
            invoke #acc AckAccountBlocked
            invoke #card EatCard
          on @("acc" :? IsAuthenticationFailed) $ do
            invoke #term ShowNoticeWrongPasscode
            invoke #acc AckAuthFailure
          on @("term" :? IsShowingNoticeWrongPasscode) $ do
            label #showingNoticeWrongPasscode -- continue
          on @(IsUserLoggedIn "acc") $ do
            caseOf $ do
              on @("term" :? IsPasscodeProvided) $ do
                invoke #term SelectService
              on @("term" :? IsSelectingService) $ do
                label #selectingService -- continue 
              on @("term" :? IsQuitSelected) $ do
                invoke #term (ShowNoticeEjectingCard)
                invoke #card EjectCard
                invoke #acc Logout
              on @("acc" :? IsUserAuthenticated :&& "term" :? IsBalanceSelected) $ do
                invoke #acc QueryBalance 
              on @("acc" :? IsUserAuthenticated :&&  "term" :? IsWithdrawalSelected) $ do
                amnt <- opRes #term withdrawalAmount
                invoke #acc (ReserveFunds amnt)
              on @("acc" :? IsQueringBalance :|| "acc" :? IsReservingFunds) $ do  
                on @("term" :? IsRequestCancelled) $ do
                  label #reqCancelled
                  invoke #acc CancelReq
                  invoke #term AckRequestCancellation
              on @("acc" :? IsBalanceAvailable :&& "term" :? IsBalanceSelected) $ do
                blnc <- opRes #acc accountBalance
                invoke #acc AckBalance
                invoke #term (ShowBalance blnc)
              on @("term" :? IsShowingBalance :|| "term" :? IsShowingNoticeAndSelect) $ 
                label #willGoToSelect
              on @("acc" :? IsFundsReservationFailed :&& "term" :? IsWithdrawalSelected) $ do
                invoke #acc AckReserveFailure
                invoke #term (ShowNoticeAndSelect "Cannot withdraw this amount")
              on @("acc" :? IsFundsReserved :&& "term" :? IsWithdrawalSelected :&& (Not (By "dispTimer"))) $ do
                amnt <- opRes #term withdrawalAmount
                invoke #term (ShowWithdrawInstruction amnt)
                invoke #dsp (DispenseCash amnt)
                newRes #dispTimer TimerRes
                invoke #dispTimer (StartTimer dispenseTimeout)
              on @("acc" :? IsFundsReserved :&& "dsp" :? IsCashCollected) $ do
                on @(By "dispTimer") (clear #dispTimer)
                invoke #dsp AckCollected
                invoke #acc ConfirmWithdrawal
                invoke #term SelectService
              on @("acc" :? IsFundsReserved :&& "dsp" :? IsDispensing :&& "dispTimer" :? IsTimerArmed) $ do
                label #waitingForCashCollect
              on @("dispTimer" :? IsTimerTriggered :&& "dsp" :? IsDispensing) $ do
                label #timeout
                clear #dispTimer
                invoke #dsp RollbackCash  
              on @("dsp" :? IsRolledBack :&& "acc" :? IsFundsReserved) $ do
                invoke #dsp AckRolledBack
                invoke #acc RollbackWithdrawal
                invoke #term (ShowNoticeAndQuit "Withdrawal timeout. Keeping your card")
                invoke #card EatCard
                invoke #acc Logout
                -- TODO: add logout
              on @("dsp" :? IsRollingBack :&& "acc" :? IsFundsReserved) $ do
                label #rollingBack
              defCase $ label #defCaseLoggedIn    -- empty
          defCase $ label #defCaseInserted        -- empty
      on @("card" :? IsEjectingCard) $ do
        label #ejectingCard  
      on @("card" :? IsEatingCard) $ do
        label #eatingCard  
      on @("card" :? IsCardReaderReleased :&& "dsp" :? IsDispenserIdle) $ do
        caseOf $ do
          on @("term" :? IsReleasingTerminal) $
            label #releasingTerminal
          on @(Not ("term" :? IsTerminalIdle)) $ do --why it does not work??
          --on @("term" :? IsShowingInvalidCardNotice :|| "term" :? IsShowingNoticeAndQuit :|| "term" :? IsShowingNoticeEjectingCard :|| "term" :? IsShowingAccountBlockedNotice) $ do 
            invoke #term ReleaseTerminal
          defCase $ do    
            label #enableCardReader
            invoke #card EnableCardReader
      defCase $ label #defCase  
  label #exit
                                  {-
*** CorePrep [Beseder.Atm.AtmLoopApp]:
Result size of CorePrep
  = {terms: 19,038,
     types: 11,757,936,
     coercions: 783,976,960,
     joins: 0/4,827}
..
(36283.64 secs,)     
-}  
-- :t evalSTransDataNamedLabels' #defCaseInserted atmAppLoopData (Proxy @(IdleState IO () () () () ()))
-- :t getLabel' #allStates atmAppLoopData (Proxy @(IdleState IO () () () () ()))
-- :t getSTransDiagramSymbol' atmAppLoopData   (Proxy @(IdleState IO () () () () ()))
-- :t getSTransDiagramStates' atmAppLoopData (Proxy @(IdleState IO () () () () ()))
-- :t evalSTransDataNamedLabels' #defCase atmAppLoopData (Proxy @(IdleState IO () () () () ()))
-- :t getLabel' #defCase atmAppLoopData (Proxy @(IdleState IO () () () () ()))
-- :t validateSTransData' atmAppLoopData (Proxy @(IdleState IO () () () () ()))
-- intr :: (_) => STrans (ContT Bool) TaskQ NoSplitter (IdleState TaskQ resDsp resCard resTerm resPace resAcc) _ _ _ ()  
-- intr = interpret atmAppLoopData   --  undefined --
