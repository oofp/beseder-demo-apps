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
import           Haskus.Utils.Variant
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

class Monad m => Terminal m res where
  data  TerminalIdle m res 
  data  GettingPasscode m res 
  data  PasscodeProvided m res
  data  PasscodeCancelled m res
  data  SelectingService m res
  data  BalanceSelected m res
  data  WithdrawalSelected m res
  data  QuitSelected m res
  data  ShowingBalance m res
  data  ShowingWithdrawInstruction m res
  data  ShowingNoticeAndQuit m res
  data  ResPar m res 

  newTerminal :: MkResDef m (ResPar m res) (TerminalIdle m res)
  getPasscode :: RequestDef m GetPasscode (TerminalIdle m res) '[GettingPasscode m res]  
  selectService :: RequestDef m SelectService (PasscodeProvided m res) '[SelectingService m res]  
  showBalance :: RequestDef m ShowBalance (BalanceSelected m res) '[ShowingBalance m res]  
  showWithdrawInstruction :: RequestDef m ShowWithdrawInstruction (WithdrawalSelected m res) '[ShowingWithdrawInstruction m res]  
  selectServiceAgain :: RequestDef m SelectService (ShowingWithdrawInstruction m res) '[SelectingService m res]  
  showNoticeAndQuitW  :: RequestDef m ShowNoticeAndQuit (ShowingWithdrawInstruction m res) '[SelectingService m res]    

  passTransition :: TransitionDef m (GettingPasscode m res) '[PasscodeProvided m res, PasscodeCancelled m res]
  selectTransition :: TransitionDef m (SelectingService m res) '[BalanceSelected m res, WithdrawalSelected m res, QuitSelected m res]
  balanceCancelTransition :: TransitionDef m (BalanceSelected m res) '[SelectingService m res]
  withdrawalCancelTransition :: TransitionDef m (WithdrawalSelected m res) '[SelectingService m res]
  balanceTransition :: TransitionDef m (ShowingBalance m res) '[SelectingService m res]
  showQuitTransition :: TransitionDef m (ShowingNoticeAndQuit m res) '[TerminalIdle m res]

  termSession :: TermDef m (TerminalIdle m res)

  _passcode :: PasscodeProvided m res -> m PassCode
  _withdrawalAmount :: WithdrawalSelected m res -> m Funds

--  
buildRes ''Terminal

type instance StateTrans (StPasscodeProvided m res name) = 'Static
--type instance StateTrans (StBalanceSelected m res name) = 'Static
type instance StateTrans (StShowingWithdrawInstruction m res name) = 'Static
type instance StateTrans (StQuitSelected m res name) = 'Static

passcode :: (Terminal m res) => StPasscodeProvided m res name -> m PassCode
passcode (St st) = _passcode st

withdrawalAmount :: (Terminal m res) => StWithdrawalSelected m res name -> m Funds
withdrawalAmount (St st) = _withdrawalAmount st
