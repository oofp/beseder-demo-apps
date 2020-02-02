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

module Beseder.Atm.Resources.Impl.CardReaderUI where

import           Protolude    
import           Beseder.Base.Base
import           Beseder.Base.Common
import           Beseder.Resources.ResourceDef
import           Beseder.Atm.Resources.Types.Domain
import           Beseder.Atm.Resources.CardReaderRes
import qualified Beseder.Atm.Resources.CardReaderRes as CardReader
import           Beseder.SDUI.SDUIResImpl
import qualified Beseder.SDUI.SDUIRes as SDUIRes
import           Beseder.SDUI.SDUIRes
import           Beseder.SDUI.SDUIContext
import           SDUI.Data.SDUIData 
import           Control.Concurrent.STM.TVar
import           Haskus.Utils.Variant
import           Data.Coerce
-- import           Control.Newtype

data UICardReader = UICardReader deriving (Eq, Show)
-- type Newtype3 b a = Newtype a b 
instance (TaskPoster m, SDUIRes m UI) => CardReader m UICardReader where
  newtype CardReaderIdle m UICardReader = CardReaderIdle (UIDyn m) -- deriving (Newtype3 (UIDyn m))
  newtype CardInserted m UICardReader = CardInserted (UIStatic m)
  newtype CardInvalid m UICardReader = CardInvalid (UIStatic m)
  newtype EjectingCard m UICardReader = EjectingCard (UIDyn m)
  newtype EatingCard m UICardReader = EatingCard (UIDyn m)
  newtype CardReaderReleased m UICardReader = CardReaderReleased (UIStatic m)
  newtype ResPar m UICardReader = MkUICard UIParams

  newCardReader :: CardReader.ResPar m UICardReader -> m (CardReaderIdle m UICardReader)
  newCardReader (MkUICard uiParams) = do
    let uiResPar :: SDUIRes.ResPar m UI
        uiResPar = MkUI uiParams
    uiInitilized <- fmap (stFromName #ui) (mkRes uiResPar)
    let uiCard = undefined
    uiShowing <- request (ShowDyn uiCard) uiInitilized
    return  $ CardReaderIdle (variantToValue uiShowing) 

  enableCardReader EnableCardReader (CardReaderReleased uiRes) = do
    let uiCard = undefined
    reqFromDynUI2 (ShowDyn uiCard) uiRes
    --newUIState <- request (ShowDyn uiCard) uiRes
    --return $ coerceVar newUIState 
    --return $ variantFromValue (CardReaderIdle (variantToValue newUIState))
    --fmap (variantToValue) (reqfromDynUI (ShowDyn uiCard) uiRes)

  ejectingTransition (EjectingCard uiRes) lst =
    void $ next uiRes 
      (\uiRespStateVar -> 
          do
            nextUIState <- handleUIResponse uiRespStateVar
            lst (variantFromValue (CardReaderReleased nextUIState))
            return True) 
  eatingTransition (EatingCard uiRes) cb = 
    let rlsCard :: UICard 
        rlsCard = undefined
    in transFromUIDyn uiRes (request (ShowStatic rlsCard)) cb       

handleUIResponse :: TaskPoster m => V  '[St (UIRespReceived m UI) "ui"] -> m (UIStatic m) 
handleUIResponse uiRespRcvdVar = do
  let uiRespRcvd = variantToValue uiRespRcvdVar 
  gotResp <- uiResp uiRespRcvd
  let newCard :: UICard 
      newCard = undefined
  fmap variantToValue (request (ShowStatic newCard) uiRespRcvd)     

transFromUIDyn :: 
  ( CoerceVar xs ys
  , TaskPoster m
  ) => UIDyn m -> (StUIRespReceived m UI "ui" -> m (V xs)) -> (V ys -> m ()) -> m ()
transFromUIDyn uiDyn respToStates cbFunc =
  void $ next uiDyn (\uiRespReceivedVar -> do
      --uiRsp <- uiResp (variantToValue uiRespReceivedVar)
      let respReceived =  variantToValue uiRespReceivedVar
      nextStates <- respToStates respReceived 
      cbFunc (coerceVar nextStates)
      return True)

{-
    --(:: RequestDef m EnableCardReader (CardReaderReleased m UICardReader) '[CardReaderIdle m UICardReader]  
  ejectCard :: RequestDef m EjectCard (CardInserted m UICardReader) '[EjectingCard m UICardReader]  
  eatCard :: RequestDef m EatCard (CardInserted m UICardReader) '[EatingCard m UICardReader]  
  ackInvalidCard :: RequestDef m AckInvalidCard (CardInvalid m UICardReader) '[EjectingCard m UICardReader]  
  releaseReader :: RequestDef m ReleaseReader (CardReaderIdle m UICardReader) '[CardReaderReleased m UICardReader]  
  cardDetectedTransition :: TransitionDef m (CardReaderIdle m UICardReader) '[CardInserted m UICardReader, CardInvalid m UICardReader]
  ejectingTransition :: TransitionDef m (EjectingCard m UICardReader) '[CardReaderReleased m UICardReader]
  eatingTransition :: TransitionDef m (EatingCard m UICardReader) '[CardReaderReleased m UICardReader]
  termReader :: TermDef m (CardReaderReleased m UICardReader)

  _cardDetails :: CardInserted m UICardReader -> m CardDetails
  -}
--  

uiResParams :: (Coercible resParams UIParams, Monad m) => resParams -> m (SDUIRes.ResPar m UI) -- (CardReaderIdle m UICardReader)
uiResParams resParams = do
  let uiParams :: UIParams
      uiParams = coerce resParams
  let uiResPar :: SDUIRes.ResPar m UI
      uiResPar = MkUI uiParams
  return uiResPar


newCardReaderFromUI :: (Coercible resParams UIParams, TaskPoster m) => resParams -> m (CardReaderIdle m UICardReader)
newCardReaderFromUI resParams = do
  uiResPar <- uiResParams resParams
  let uiCard = undefined
  uiInitilized <- fmap (stFromName #ui) (mkRes uiResPar)
  uiShowing <- request (ShowDyn uiCard) uiInitilized
  return  $ CardReaderIdle (variantToValue uiShowing) 

newResFromUI :: 
  ( Coercible resParams UIParams
  , TaskPoster m
  , Request m initUIReq (St (UIInitialized m UI) "ui")
  , ReqResult initUIReq (St (UIInitialized m UI) "ui") ~ '[a]
  , Coercible a initResState) => initUIReq -> resParams -> m initResState
newResFromUI initUIReq resParams = do
  uiResPar <- uiResParams resParams
  uiInitilized <- fmap (stFromName #ui) (mkRes uiResPar)
  uiShowing <- request initUIReq uiInitilized
  return  $ coerce (variantToValue uiShowing) 

newCardReaderFromUI2 :: TaskPoster m => CardReader.ResPar m UICardReader -> m (CardReaderIdle m UICardReader)
newCardReaderFromUI2 resParams = newResFromUI (ShowDyn undefined) resParams 

-- deriving instance Newtype (CardReaderIdle m UICardReader) (UIDyn m)
{-
reqfromDynUI :: 
  ( Coercible curRes (UIDyn m)
  , Request m uiReq (UIDyn m) 
  , '[newUIState] ~ ReqResult uiReq (UIDyn m)
  , Coercible newUIState newRes
  , _
  ) => uiReq -> curRes -> m (V '[newRes])
reqfromDynUI uiReq curRes  = do
  let curUIState :: UIDyn m 
      curUIState = coerce curRes
  newUIState :: newUIState <- request uiReq curUIState
  return $ variantFromValue $ coerce $ variantToValue newUIState
-}
--getNextUIs (uiResp -> V nextUIs) -> 

{-
class ReqFromUI m resState uiState req resState2 where -- | uiState resState req -> resState2  where
  reqFromUI :: Proxy uiState -> resState -> req -> m resState2

instance 
  ( Coercible resState uiState
  , Request m uiReq uiState 
  , '[newUIState] ~ ReqResult uiReq uiState
  , Coercible newUIState resState2
  ) => ReqFromUI m resState uiState uiReq resState2 where
  -- reqFromUI :: forall uiState resState resState2 uiReq m. resState -> uiReq -> m resState2
  reqFromUI _px resState uiReq = do
    let curUIState :: newUIState
        curUIState = coerce resState
    newUIState <- request uiReq curUIState
    return $ variantFromValue $ coerce $ variantToValue newUIState
-}

{-
reqfromDynUI :: 
  ( Coercible curRes uiState
  , Request m uiReq uiState 
  , '[newUIState] ~ ReqResult uiReq uiState
  ) => uiReq -> Proxy uiState -> curRes -> m _ -- (uiState) -- (newUIState -> newRes) -> m (V '[newRes])
reqfromDynUI uiReq _ curRes = do
  let curUIState = coerce curRes
  newUIState <- request uiReq curUIState
  --return $ variantFromValue $ f $ variantToValue newUIState
  }
  return newUIState
-}

{-
reqfromDynUI :: 
  ( Request m uiReq uiState 
  , '[newUIState] ~ ReqResult uiReq uiState
  , Coercible newUIState newResSt
  ) => uiReq -> uiState -> m (V '[newResSt]) -- (uiState) -- (newUIState -> newRes) -> m (V '[newRes])
reqfromDynUI uiReq uiState = do
  newUIState <- request uiReq uiState
  return $ variantFromValue $ coerce $ variantToValue newUIState
-}

reqfromDynUI :: 
  ( Request m uiReq uiState 
  , '[newUIState] ~ ReqResult uiReq uiState
  , Coercible newUIState newResSt
  ) => uiReq -> uiState -> m (newResSt) -- (uiState) -- (newUIState -> newRes) -> m (V '[newRes])
reqfromDynUI uiReq uiState = do
  newUIState <- request uiReq uiState
  return $ coerce $ variantToValue newUIState

reqFromDynUI2 :: (Request m uiReq uiRes, CoerceVar (ReqResult uiReq uiRes) ys) => uiReq -> uiRes -> m (V ys) 
reqFromDynUI2 uiReq uiRes = do
  newUIState <- request uiReq uiRes
  return $ coerceVar newUIState 
