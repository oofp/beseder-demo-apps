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

module Beseder.Atm.Resources.UIImpl.CashDispenserUI where

import           Protolude    
import           Haskus.Utils.Variant
import           Beseder.Base.Base
import           Beseder.Base.Common
-- import           Beseder.Atm.Resources.Types.Domain
import           Beseder.Atm.Resources.CashDispenserRes
import           Beseder.SDUI.SDUIResImpl
import           Beseder.SDUI.SDUIRes
import           Beseder.SDUI.SDUIHelper
import           SDUI.Data.Form  

data UICashDisp = UICashDisp deriving (Eq, Show)

instance (TaskPoster m, SDUIRes m UI) => CashDispenser m UICashDisp where
  newtype  DispenserIdle m UICashDisp = DispenserIdle (UIStaticData m) 
  newtype  Dispensing m UICashDisp = Dispensing (UIDynData  m)
  newtype  CashCollected m UICashDisp = CashCollected (UIStaticData m)
  newtype  RollingBack m UICashDisp = RollingBack (UIDynData m)
  newtype  RolledBack m UICashDisp = RolledBack (UIStaticData m)
  newtype ResPar m UICashDisp = MkUICashDisp UIParams

  newCashDispenser = newResFromUIData
  
  dispense (DispenseCash _) (DispenserIdle st) = reqUI1 st
  rollback RollbackCash (Dispensing st) = reqUI1 st
  ackRoolback AckRolledBack (RolledBack st) = reqUI1 st
  ackCollected AckCollected (CashCollected st) = reqUI1 st
  dispensingTransition = uiTrans 
  rollbackTransition = uiTrans
  termDispenser = termUI 

instance GetFormEntries (CashCollected m UICashDisp) where
  getFormEntris _ = []

instance GetFormEntries (RolledBack m UICashDisp) where
  getFormEntris _ = []
