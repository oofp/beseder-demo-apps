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

data UICashDisp = UICashDisp deriving (Eq, Show)

idleReq :: ShowStatic
idleReq = showNotice "Dispenser is idle"

collectedReq :: ShowStatic
collectedReq = showNotice "Cash collected"

instance (TaskPoster m, SDUIRes m UI) => CashDispenser m UICashDisp where
  newtype  DispenserIdle m UICashDisp = DispenserIdle (UIStatic m) 
  newtype  Dispensing m UICashDisp = Dispensing (UIDyn  m)
  newtype  CashCollected m UICashDisp = CashCollected (UIStatic m)
  newtype  RollingBack m UICashDisp = RollingBack (UIDyn m)
  newtype  RolledBack m UICashDisp = RolledBack (UIStatic m)
  newtype ResPar m UICashDisp = MkUICashDisp UIParams

  newCashDispenser param = newResFromUI idleReq param
  
  dispense (DispenseCash _) (DispenserIdle uiRes) = reqFromUI (showButtons "Dispensing:" ["Collected"]) uiRes
  rollback RollbackCash (Dispensing uiRes) = reqFromUI (showButtons "Rollingback:" ["Rolledback","Still Collected"]) uiRes
  ackRoolback AckRolledBack (RolledBack uiRes) = reqFromUI idleReq uiRes
  ackCollected AckCollected (CashCollected uiRes) = reqFromUI idleReq uiRes
  dispensingTransition (Dispensing uiRes) cbFunc = transFromUI1 uiRes collectedReq cbFunc 
  rollbackTransition (RollingBack uiRes) cbFunc = transFromBtn2 uiRes "Rolledback" (showNotice "Rolled back") collectedReq cbFunc  
  termDispenser (DispenserIdle uiRes) = termFromUI uiRes

  {-  
  ackRoolback :: RequestDef m AckRolledBack (RolledBack m UICashDisp) '[DispenserIdle m UICashDisp]  
  ackCollected :: RequestDef m AckCollected (CashCollected m UICashDisp) '[DispenserIdle m UICashDisp]  
  dispensingTransition :: TransitionDef m (Dispensing m UICashDisp) '[CashCollected m UICashDisp]
  rollbackTransition :: TransitionDef m (RollingBack m UICashDisp) '[RolledBack m UICashDisp, CashCollected m UICashDisp]  
  termDispenser :: TermDef m (DispenserIdle m UICashDisp)
  -}

