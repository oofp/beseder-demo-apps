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
{-# LANGUAGE TypeOperators         #-}

module Beseder.Atm.Resources.UIImpl.AccountUI where

import           Protolude    
import           Beseder.Base.Common
import           Beseder.Atm.Resources.Types.Domain
import           Beseder.Atm.Resources.AccountRes
import           Beseder.SDUI.SDUIResImpl
import           Beseder.SDUI.SDUIRes
import           Beseder.SDUI.SDUIHelper
import           SDUI.Data.Form
-- import           Beseder.Atm.Resources.Types.Domain

data UIAccount = UIAccount deriving (Eq, Show)

instance (TaskPoster m, SDUIRes m UI) => Account m UIAccount where
  newtype SessionIdle m UIAccount = SessionIdle (UIStaticData m)
  newtype Authenticating m UIAccount = Authenticating (UIDynData m)
  newtype UserAuthenticated m UIAccount = UserAuthenticated (UIStaticData m)
  newtype AccountBlocked m UIAccount = AccountBlocked (UIStaticData m)
  newtype AuthenticationFailed m UIAccount = AuthenticationFailed (UIStaticData m)
  newtype ReservingFunds m UIAccount = ReservingFunds (UIDynData m)
  newtype FundsReserved m UIAccount = FundsReserved (UIStaticData m)
  newtype FundsReservationFailed m UIAccount = FundsReservationFailed (UIStaticData m)
  newtype QueringBalance m UIAccount = QueringBalance (UIDynData m)
  newtype BalanceAvailable m UIAccount = BalanceAvailable (UIStaticData m)
  newtype ResPar m UIAccount = MkUIAccount UIParams

  newUserSession = newResFromUIData
  authenticate (Authenticate _ _) (SessionIdle st) = reqUI1 st  
  cancelAuthentication CancelReq (Authenticating st) = reqUI1 st
  cancelReserving CancelReq (ReservingFunds st) = reqUI1 st 
  ackAuthFailure AckAuthFailure (AuthenticationFailed st) = reqUI1 st
  reserveFunds (ReserveFunds _) (UserAuthenticated st) = reqUI1 st 
  ackReserveFailure AckReserveFailure (FundsReservationFailed st) = reqUI1 st
  confirmWithdrawal ConfirmWithdrawal (FundsReserved st) = reqUI1 st 
  rollbackWithdrawal RollbackWithdrawal (FundsReserved st) = reqUI1 st 
  queryBalance QueryBalance (UserAuthenticated st) = reqUI1 st
  cancelBalanceQuery CancelReq (QueringBalance st) = reqUI1 st 
  ackBalance AckBalance (BalanceAvailable st) = reqUI1 st  
  logout Logout (UserAuthenticated st) = reqUI1 st  
  ackAccountBlocked AckAccountBlocked (AccountBlocked st) = reqUI1 st


  authTransition = uiTrans 
  reserveTransition = uiTrans 
  balanceTransition = uiTrans 

  termSession = termUI

  _accountBalance (BalanceAvailable st) = return $ Funds $ getNumFromResp st "balance" 
  

instance GetFormEntries (BalanceAvailable m UIAccount) where
   getFormEntris _ = [FormEntry "balance" (FormGroup (FormGroupParams (Just "Enter account balance:") (Input Number) (Just "balance ammount")))]

instance GetFormEntries (FundsReserved m UIAccount) where
  getFormEntris _ = []

instance GetFormEntries (UserAuthenticated m UIAccount) where
  getFormEntris _ = []

instance GetFormEntries (AuthenticationFailed m UIAccount) where
  getFormEntris _ = []

instance GetFormEntries (FundsReservationFailed m UIAccount) where
  getFormEntris _ = []

instance GetFormEntries (AccountBlocked m UIAccount) where
  getFormEntris _ = []
