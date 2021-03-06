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

module Beseder.Atm.Resources.TerminalRes where

import           Protolude    
import           Beseder.Base.Base
import           Beseder.Base.Common
import           Beseder.Resources.ResourceDef
import           Beseder.Atm.Resources.Types.Domain

data GetPasscode = GetPasscode deriving (Eq, Show)
data SelectService = SelectService deriving (Eq, Show)
data ShowBalance = ShowBalance Funds deriving (Eq, Show)
data ShowWithdrawInstruction = ShowWithdrawInstruction Funds deriving (Eq, Show)
data SelectServiceAgain = SelectServiceAgain deriving (Eq, Show)
data ShowNoticeAndQuit = ShowNoticeAndQuit Text deriving (Eq, Show)
data ShowNoticeAndSelect = ShowNoticeAndSelect Text deriving (Eq, Show)
data ShowNoticeEjectingCard = ShowNoticeEjectingCard deriving (Eq, Show)
data ShowNoticeWrongPasscode = ShowNoticeWrongPasscode deriving (Eq, Show)
data ShowAccountBlockedNotice = ShowAccountBlockedNotice deriving (Eq, Show)
data ShowInvalidCardNotice = ShowInvalidCardNotice deriving (Eq, Show)
data ReleaseTerminal = ReleaseTerminal deriving (Eq, Show)
data AckRequestCancellation = AckRequestCancellation deriving (Eq, Show)

data PassCodeProp
data WithdrawalAmountProp

type instance PropType PassCodeProp = PassCode 
type instance PropType WithdrawalAmountProp = Funds 

class Monad m => Terminal m res where
  data  TerminalIdle m res 
  data  GettingPasscode m res 
  data  PasscodeProvided m res
  data  PasscodeCancelled m res
  data  SelectingService m res
  data  BalanceSelected m res
  data  WithdrawalSelected m res
  data  QuitSelected m res
  data  RequestCancelled m res
  data  ShowingBalance m res
  data  ShowingWithdrawInstruction m res
  data  ShowingNoticeAndQuit m res
  data  ShowingNoticeAndSelect m res
  data  ShowingNoticeEjectingCard m res
  data  ShowingNoticeWrongPasscode m res
  data  ShowingAccountBlockedNotice m res
  data  ShowingInvalidCardNotice m res 
  data  ReleasingTerminal m res
  data  ResPar m res 

  newTerminal :: MkResDef m (ResPar m res) (TerminalIdle m res)
  getPasscode :: RequestDef m GetPasscode (TerminalIdle m res) '[GettingPasscode m res]  
  selectService :: RequestDef m SelectService (PasscodeProvided m res) '[SelectingService m res]  
  showBalance :: RequestDef m ShowBalance (BalanceSelected m res) '[ShowingBalance m res]  
  showWithdrawInstruction :: RequestDef m ShowWithdrawInstruction (WithdrawalSelected m res) '[ShowingWithdrawInstruction m res]  
  selectServiceAgain :: RequestDef m SelectService (ShowingWithdrawInstruction m res) '[SelectingService m res]  
  showNoticeAndQuitW  :: RequestDef m ShowNoticeAndQuit (ShowingWithdrawInstruction m res) '[ShowingNoticeAndQuit m res]    
  showNoticeAndSelect  :: RequestDef m ShowNoticeAndSelect (WithdrawalSelected m res) '[ShowingNoticeAndSelect m res]    
  showNoticeEjecting  :: RequestDef m ShowNoticeEjectingCard (QuitSelected m res) '[ShowingNoticeEjectingCard m res]    
  showNoticeWrongPasscode  :: RequestDef m ShowNoticeWrongPasscode (PasscodeProvided m res) '[ShowingNoticeWrongPasscode m res]    
  showAccountBlockedNotice  :: RequestDef m ShowAccountBlockedNotice (PasscodeProvided m res) '[ShowingAccountBlockedNotice m res]    
  ackRequestCancellation :: RequestDef m AckRequestCancellation (RequestCancelled m res) '[SelectingService m res]    
  showNoticeAndQuitCancelled  :: RequestDef m ShowNoticeEjectingCard (PasscodeCancelled m res) '[ShowingNoticeEjectingCard m res]    
  showInvalidCardNotice :: RequestDef m ShowInvalidCardNotice (TerminalIdle m res) '[ShowingInvalidCardNotice m res]    
  releaseTerminalInvalid :: RequestDef m ReleaseTerminal (ShowingInvalidCardNotice m res) '[ReleasingTerminal m res]
  releaseTerminalBlocked :: RequestDef m ReleaseTerminal (ShowingAccountBlockedNotice m res) '[ReleasingTerminal m res]
  releaseTerminalEjecting :: RequestDef m ReleaseTerminal (ShowingNoticeEjectingCard m res) '[ReleasingTerminal m res]
  releaseShowingNoticeAndQuit :: RequestDef m ReleaseTerminal (ShowingNoticeAndQuit m res) '[ReleasingTerminal m res]
  
  passTransition :: TransitionDef m (GettingPasscode m res) '[PasscodeProvided m res, PasscodeCancelled m res]
  selectTransition :: TransitionDef m (SelectingService m res) '[BalanceSelected m res, WithdrawalSelected m res, QuitSelected m res]
  balanceCancelTransition :: TransitionDef m (BalanceSelected m res) '[RequestCancelled m res]
  withdrawalCancelTransition :: TransitionDef m (WithdrawalSelected m res) '[RequestCancelled m res]
  balanceTransition :: TransitionDef m (ShowingBalance m res) '[SelectingService m res]
  showNoticeAndSelectTransition :: TransitionDef m (ShowingNoticeAndSelect m res) '[SelectingService m res]
  -- showQuitTransition :: TransitionDef m (ShowingNoticeAndQuit m res) '[TerminalIdle m res]
  showWrongPasscodeTransition :: TransitionDef m (ShowingNoticeWrongPasscode m res) '[GettingPasscode m res]
  --showAccountBlockedTransition :: TransitionDef m (ShowingAccountBlockedNotice m res) '[TerminalIdle m res]
  --showInvalidCardTransition ::  TransitionDef m (ShowingInvalidCardNotice m res) '[TerminalIdle m res] 
  --showingEjectingCardTransition ::  TransitionDef m (ShowingNoticeEjectingCard m res) '[TerminalIdle m res] 
  releasingTerminalTransition :: TransitionDef m (ReleasingTerminal m res) '[TerminalIdle m res] 
  termSession :: TermDef m (TerminalIdle m res)

  _passcode :: PasscodeProvided m res -> PassCode
  _withdrawalAmount :: WithdrawalSelected m res -> Funds

--  
buildRes ''Terminal

passcode :: forall res m name. (Terminal m res) => StPasscodeProvided m res name -> m PassCode
passcode (St st) = return $ _passcode st

withdrawalAmount :: forall res m name. (Terminal m res) => StWithdrawalSelected m res name -> m Funds
withdrawalAmount (St st) = return $ _withdrawalAmount st

instance Terminal m res => Property  (StPasscodeProvided m res name) PassCodeProp where
  getProp (St st) _px = _passcode st

instance Terminal m res => Property  (StWithdrawalSelected m res name) WithdrawalAmountProp where
  getProp (St st) _px = _withdrawalAmount st
