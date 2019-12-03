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

module Beseder.Atm.Resources.AccountRes where

import           Protolude    
import           Haskus.Utils.Variant
import           Beseder.Base.Base
import           Beseder.Base.Common
import           Beseder.Resources.ResourceDef
import           Beseder.Atm.Resources.Types.Domain

data Authenticate = Authenticate CardDetails PassCode deriving (Eq, Show)
data Logout = Logout deriving (Eq, Show)
data ReserveFunds = ReserveFunds deriving (Eq, Show) 
data ConfirmWithdrawal = ConfirmWithdrawal deriving (Eq, Show) 
data RollbackWithdrawal = RollbackWithdrawal deriving (Eq, Show) 
data AckFailure = AckFailure deriving (Eq, Show)
data CancelReq = CancelReq deriving (Eq, Show)

class Monad m => Account m res where
  data  SessionIdle m res 
  data  Authenticating m res 
  data  UserAuthenticated m res 
  data  AccountBlocked m res 
  data  AuthenticationFailed m res 
  data  ReservedingFunds m res 
  data  FundsReserved m res 
  data  FundsReservationFailed m res 

  data  ResPar m res 

  newUserSession :: MkResDef m (ResPar m res) (SessionIdle m res)
  authenticate :: RequestDef m Authenticate (SessionIdle m res) '[Authenticating m res]  
  cancelAuthentication :: RequestDef m CancelReq (Authenticating m res) '[SessionIdle m res]  
  cancelReserving :: RequestDef m CancelReq (ReservedingFunds m res) '[UserAuthenticated m res]  
  ackAuthFailure :: RequestDef m AckFailure (AuthenticationFailed m res) '[SessionIdle m res]  
  ackReservFailure :: RequestDef m AckFailure (FundsReservationFailed m res) '[UserAuthenticated m res]  
  logout :: RequestDef m Logout (UserAuthenticated m res) '[SessionIdle m res]  

  authTransition :: TransitionDef m (Authenticating m res) '[UserAuthenticated m res, AuthenticationFailed m res, AccountBlocked m res]
  termSession :: TermDef m (SessionIdle m res)

--  
buildRes ''Account

