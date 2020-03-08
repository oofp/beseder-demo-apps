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
{-# LANGUAGE FunctionalDependencies #-}
-- {-# LANGUAGE StandaloneDeriving    #-}
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- {-# LANGUAGE ConstraintKinds #-}

module Beseder.Atm.Resources.UIImpl.CardReaderUI where

import           Protolude    
import           Beseder.Base.Common
import           Beseder.Atm.Resources.Types.Domain
import           Beseder.Atm.Resources.CardReaderRes
import           Beseder.SDUI.SDUIResImpl
import           Beseder.SDUI.SDUIRes
import           Beseder.SDUI.SDUIHelper
import           SDUI.Data.Form
-- import           Beseder.Atm.Resources.Types.Domain

data UICardReader = UICardReader deriving (Eq, Show)
-- type Newtype3 b a = Newtype a b 
instance (TaskPoster m, SDUIRes m UI) => CardReader m UICardReader where
  newtype CardReaderIdle m UICardReader = CardReaderIdle (UIDynData m) -- deriving (Newtype3 (UIDyn m))
  newtype CardInserted m UICardReader = CardInserted (UIStaticData m)
  newtype CardInvalid m UICardReader = CardInvalid (UIStaticData m)
  newtype EjectingCard m UICardReader = EjectingCard (UIDynData m)
  newtype EatingCard m UICardReader = EatingCard (UIDynData m)
  newtype CardReaderReleased m UICardReader = CardReaderReleased (UIStaticData m)
  newtype ResPar m UICardReader = MkUICard UIParams

  newCardReader = newResFromUIData
  enableCardReader EnableCardReader (CardReaderReleased st) = reqUI1 st
  ejectCard EjectCard (CardInserted st) = reqUI1 st
  eatCard EatCard (CardInserted st) = reqUI1 st
  ackInvalidCard AckInvalidCard (CardInvalid st) = reqUI1 st
  releaseReader ReleaseReader (CardReaderIdle st) = reqUI1 st

  ejectingTransition = uiTrans 
  eatingTransition = uiTrans 
  cardDetectedTransition = uiTrans

  termReader = termUI

  _cardDetails (CardInserted st) = CardDetails $ getTextFromResp st "cardDetails" 

instance GetFormEntries (CardInvalid m UICardReader) where
  getFormEntris _ = []

instance GetFormEntries (CardInserted m UICardReader) where
   getFormEntris _ = [FormEntry "cardDetails" (FormGroup (FormGroupParams (Just "Enter card number") (Input Text) (Just "valid card number")))]

instance GetFormEntries (CardReaderReleased m UICardReader) where
  getFormEntris _ = []

