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
{-# LANGUAGE DeriveAnyClass        #-}

module Beseder.Atm.Resources.UIImpl.CashDispenserUI where

import           Protolude    
import           Beseder.Base.Common
-- import           Beseder.Atm.Resources.Types.Domain
import           Beseder.Atm.Resources.CashDispenserRes
import           Beseder.SDUI.SDUIResImpl
import           Beseder.SDUI.SDUIRes
import           Beseder.SDUI.SDUIHelper

data UICashDisp = UICashDisp deriving (Eq, Show)

instance (TaskPoster m, SDUIRes m UI) => CashDispenser m UICashDisp where
  newtype  DispenserIdle m UICashDisp = DispenserIdle (UIStaticData m) deriving GetFormEntries
  newtype  Dispensing m UICashDisp = Dispensing (UIDynData  m) deriving GetFormEntries
  newtype  CashCollected m UICashDisp = CashCollected (UIStaticData m) deriving GetFormEntries
  newtype  RollingBack m UICashDisp = RollingBack (UIDynData m) deriving GetFormEntries
  newtype  RolledBack m UICashDisp = RolledBack (UIStaticData m) deriving GetFormEntries
  newtype ResPar m UICashDisp = MkUICashDisp UIParams

  newCashDispenser = newResFromUIData
  
  dispense (DispenseCash _) (DispenserIdle st) = reqUI1 st
  rollback RollbackCash (Dispensing st) = reqUI1 st
  ackRoolback AckRolledBack (RolledBack st) = reqUI1 st
  ackCollected AckCollected (CashCollected st) = reqUI1 st
  dispensingTransition = uiTrans 
  rollbackTransition = uiTrans
  termDispenser = termUI 

