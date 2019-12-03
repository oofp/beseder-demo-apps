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

module Beseder.Atm.Resources.CardReaderRes where

import           Protolude    
import           Beseder.Base.Base
import           Beseder.Base.Common
import           Beseder.Resources.ResourceDef
import           Beseder.Atm.Resources.Types.Domain

data EjectCard = EjectCard deriving (Eq, Show)
data EatCard = EatCard deriving (Eq, Show)
data ReleaseReader = ReleaseReader deriving (Eq, Show)

class Monad m => CardReader m res where
  data  CardReaderIdle m res 
  data  CardInserted m res 
  data  CardInvalid m res 
  data  EjectingCard m res 
  data  EatingCard m res 
  data  CardReaderReleased m res 
  data  ResPar m res 

  newCardReader :: MkResDef m (ResPar m res) (CardReaderIdle m res)
  ejectCard :: RequestDef m EjectCard (CardInserted m res) '[EjectingCard m res]  
  ejectInvalidCard :: RequestDef m EjectCard (CardInvalid m res) '[EjectingCard m res]  
  eatCard :: RequestDef m EatCard (CardInserted m res) '[EatingCard m res]  
  releaseReader :: RequestDef m ReleaseReader (CardReaderIdle m res) '[CardReaderReleased m res]  
  cardDetectedTransition :: TransitionDef m (CardReaderIdle m res) '[CardInserted m res, CardInvalid m res]
  ejectingTransition :: TransitionDef m (EjectingCard m res) '[CardReaderIdle m res]
  eatingTransition :: TransitionDef m (EatingCard m res) '[CardReaderIdle m res]
  termReader :: TermDef m (CardReaderReleased m res)

  _getCardDetails :: CardInserted m res -> m CardDetails
  
--  
buildRes ''CardReader

-- manual additions
type instance TermRequest (StCardReaderIdle m res name) = ReleaseReader
type instance StateTrans (StCardInvalid m res name) = 'Static

getCardDetails :: CardReader m res => StCardInserted m res name -> m CardDetails
getCardDetails (St cardInserted) = _getCardDetails cardInserted
