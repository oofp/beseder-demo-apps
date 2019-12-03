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

module Beseder.Atm.Resources.CashDispenserRes where

import           Protolude    
import           Haskus.Utils.Variant
import           Beseder.Base.Base
import           Beseder.Base.Common
import           Beseder.Resources.ResourceDef

data DispenseCash   = DispenseCash deriving (Eq, Show)
data RollbackCash   = RollbackCash deriving (Eq, Show)
data AckCollected   = AckCollected deriving (Eq, Show)
data AckRolledBack  = AckRolledBack deriving (Eq, Show)

data ReleaseDispenser = ReleaseDispenser deriving (Eq, Show)
  
class Monad m => CashDispenser m res where
  data  DispenserIdle m res 
  data  Dispensing m res 
  data  CashCollected m res 
  data  RollingBack m res 
  data  RolledBack m res 
  data  ResPar m res 

  newCashDispenser :: MkResDef m (ResPar m res) (DispenserIdle m res)
  dispense :: RequestDef m DispenseCash (DispenserIdle m res) '[Dispensing m res]  
  rollback :: RequestDef m RollbackCash (Dispensing m res) '[RolledBack m res, CashCollected m res]  
  ackRoolback :: RequestDef m AckRolledBack (RolledBack m res) '[DispenserIdle m res]  
  ackCollected :: RequestDef m AckCollected (RolledBack m res) '[DispenserIdle m res]  
  dispensingTransition :: TransitionDef m (Dispensing m res) '[CashCollected m res]
  termDispenser :: TermDef m (DispenserIdle m res)

--  
buildRes ''CashDispenser

