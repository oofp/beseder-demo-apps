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
import           Haskus.Utils.Variant
import           Beseder.Base.Base
import           Beseder.Base.Common
import           Beseder.Resources.ResourceDef

data EjectCard = EjectCard deriving (Eq, Show)
data EatCard = EatCard deriving (Eq, Show)
data ReleaseReader = ReleaseReader deriving (Eq, Show)
  
class Monad m => CardReader m res where
  data  CardReaderIdle m res 
  data  CardInserted m res 
  data  EjectingCard m res 
  data  EatingCard m res 
  data  CardReaderReleased m res 
  data  ResPar m res 

  newCardReader :: MkResDef m (ResPar m res) (CardReaderIdle m res)
  ejectCard :: RequestDef m EjectCard (CardInserted m res) '[EjectingCard m res]  
  eatCard :: RequestDef m EatCard (CardInserted m res) '[EatingCard m res]  
  releaseCard :: RequestDef m EatCard (CardReaderIdle m res) '[CardReaderReleased m res]  
  cardDetectedTransition :: TransitionDef m (CardReaderIdle m res) '[CardInserted m res]
  ejectingTransition :: TransitionDef m (EjectingCard m res) '[CardReaderIdle m res]
  eatingTransition :: TransitionDef m (EatingCard m res) '[CardReaderIdle m res]
  termReader :: TermDef m (CardReaderReleased m res)
  
buildRes ''CardReader

type instance TermRequest (StCardReaderIdle m res name) = ReleaseReader
