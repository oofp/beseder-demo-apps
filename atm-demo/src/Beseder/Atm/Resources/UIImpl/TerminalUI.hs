{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TemplateHaskell       #-}

module Beseder.Atm.Resources.UIImpl.TerminalUI where

import           Protolude    
import           Beseder.Base.Common
import           Beseder.Atm.Resources.Types.Domain
import           Beseder.Atm.Resources.TerminalRes
import           Beseder.SDUI.SDUIResImpl
import           Beseder.SDUI.SDUIRes
import           Beseder.SDUI.SDUIHelper
import           SDUI.Data.Form  
-- import           Data.Coerce (coerce)

data UITerm = UITerm deriving (Eq, Show)

instance (TaskPoster m, SDUIRes m UI) => Terminal m UITerm where
  newtype  TerminalIdle m UITerm = TerminalIdle (UIStaticData m)
  newtype  GettingPasscode m UITerm = GettingPasscode (UIDynData m)
  newtype  PasscodeProvided m UITerm = PasscodeProvided (UIStaticData m)
  newtype  PasscodeCancelled m UITerm =PasscodeCancelled (UIStaticData m)
  newtype  SelectingService m UITerm = SelectingService (UIDynData m)
  newtype  BalanceSelected m UITerm = BalanceSelected (UIDynData m)
  newtype  WithdrawalSelected m UITerm = WithdrawalSelected (UIDynData m)
  newtype  QuitSelected m UITerm = QuitSelected (UIStaticData m)
  newtype  RequestCancelled m UITerm = RequestCancelled (UIStaticData m)
  newtype  ShowingBalance m UITerm = ShowingBalance (UIDynData m)
  newtype  ShowingWithdrawInstruction m UITerm = ShowingWithdrawInstruction (UIStaticData m)
  newtype  ShowingNoticeAndQuit m UITerm = ShowingNoticeAndQuit (UIStaticData m)
  newtype  ShowingNoticeAndSelect m UITerm = ShowingNoticeAndSelect (UIDynData m)
  newtype  ShowingNoticeEjectingCard m UITerm = ShowingNoticeEjectingCard (UIStaticData m)
  newtype  ShowingNoticeWrongPasscode m UITerm = ShowingNoticeWrongPasscode (UIDynData m)
  newtype  ShowingAccountBlockedNotice m UITerm = ShowingAccountBlockedNotice (UIStaticData m)
  newtype  ShowingInvalidCardNotice m UITerm = ShowingInvalidCardNotice (UIStaticData m)
  newtype  ReleasingTerminal m UITerm = ReleasingTerminal (UIDynData m)
  newtype  ResPar m UITerm = MkUITerm UIParams

  newTerminal = newResFromUIData 
  getPasscode GetPasscode (TerminalIdle st) = reqUI1 st
  selectService SelectService (PasscodeProvided st) = reqUI1 st
  showBalance (ShowBalance _) (BalanceSelected st) = reqUI1 st
  showWithdrawInstruction (ShowWithdrawInstruction _) (WithdrawalSelected st) = reqUI1 st
  selectServiceAgain SelectService (ShowingWithdrawInstruction st) = reqUI1 st
  showNoticeAndQuitW  (ShowNoticeAndQuit _txt) (ShowingWithdrawInstruction st) = reqUI1 st
  showNoticeAndSelect  (ShowNoticeAndSelect _txt) (WithdrawalSelected st) = reqUI1 st
  showNoticeEjecting ShowNoticeEjectingCard (QuitSelected st) = reqUI1 st
  showNoticeWrongPasscode  ShowNoticeWrongPasscode (PasscodeProvided st) = reqUI1 st
  showAccountBlockedNotice ShowAccountBlockedNotice (PasscodeProvided st) = reqUI1 st
  ackRequestCancellation AckRequestCancellation (RequestCancelled st) = reqUI1 st
  showNoticeAndQuitCancelled  ShowNoticeEjectingCard (PasscodeCancelled st) = reqUI1 st
  showInvalidCardNotice ShowInvalidCardNotice (TerminalIdle st) = reqUI1 st
  releaseTerminalInvalid ReleaseTerminal (ShowingInvalidCardNotice st) = reqUI1 st
  releaseTerminalBlocked ReleaseTerminal (ShowingAccountBlockedNotice st) = reqUI1 st
  releaseTerminalEjecting ReleaseTerminal (ShowingNoticeEjectingCard st) = reqUI1 st
  releaseShowingNoticeAndQuit ReleaseTerminal (ShowingNoticeAndQuit st) = reqUI1 st
  --releaseShowingNoticeAndQuit ReleaseTerminal= reqUI1 . coerce
  
  passTransition = uiTrans
  selectTransition = uiTrans
  balanceCancelTransition = uiTrans 
  withdrawalCancelTransition = uiTrans  
  balanceTransition = uiTrans
  showNoticeAndSelectTransition = uiTrans 
  showWrongPasscodeTransition = uiTrans 
  releasingTerminalTransition = uiTrans 
  
  termSession = termUI

  _passcode (PasscodeProvided st) = PassCode $ getTextFromResp st "passcode" 
  _withdrawalAmount (WithdrawalSelected st) = Funds $ getNumFromResp st "funds" 

instance GetFormEntries (BalanceSelected m UITerm) where
  getFormEntris _ = []

instance GetFormEntries (RequestCancelled m UITerm) where
  getFormEntris _ = []

instance GetFormEntries (SelectingService m UITerm) where
  getFormEntris _ = []

instance GetFormEntries (GettingPasscode m UITerm) where
  getFormEntris _ = []

instance GetFormEntries (QuitSelected m UITerm) where
  getFormEntris _ = []

instance GetFormEntries (TerminalIdle m UITerm) where
  getFormEntris _ = []

instance GetFormEntries (PasscodeCancelled m UITerm) where
  getFormEntris _ = []

instance GetFormEntries (PasscodeProvided m UITerm) where
   getFormEntris _ = [FormEntry "passode" (FormGroup (FormGroupParams (Just "Enter your secret code") (Input Number) (Just "digits")))]

instance GetFormEntries (WithdrawalSelected m UITerm) where
   getFormEntris _ = [FormEntry "funds" (FormGroup (FormGroupParams (Just "Enter withdrawal amount") (Input Number) (Just "20,40,60,80,100,150,200,..")))]
