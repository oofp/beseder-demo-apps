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

module Beseder.Callback.Resources.CallbackRes where

import           Protolude    
import           Beseder.Base.Base
import           Beseder.Base.Common
import           Beseder.Resources.ResourceDef

data CallbackRequest   = CallbackRequest {localNumber :: Text, remoteNumber :: Text} deriving (Eq, Show)

data CallbackProgressState
  = DialingLocal
  | RingbackLocal
  | DialingRemote
  | RingbackRemote
   deriving (Eq, Show)

data CallbackRequestFailed = CallbackRequestFailed deriving (Eq, Show)  
data CallbackAckRequest = CallbackAckRequest deriving (Eq, Show)  
data NotifyCallbackProgress   = NotifyCallbackProgress CallbackProgressState deriving (Eq, Show)
data NotifyCallbackConnected   = NotifyCallbackConnected deriving (Eq, Show)
data CallbackAckMuted   =  CallbackAckMuted deriving (Eq, Show)
data CallbackAckUnMuted   =  CallbackAckUnMuted deriving (Eq, Show)
data NotifyCallbackDisconnected   = NotifyCallbackDisconnected deriving (Eq, Show)
data ReleaseCallback = ReleaseCallback deriving (Eq, Show)

class Monad m => Callback m res where
  data  CallbackIdle m res 
  data  CallbackReleased m res 
  data  CallRequestReceived m res 
  data  CallbackFailed m res 
  data  CallbackProgress m res 
  data  CallbackConnected m res 
  data  CallbackMuted m res 
  data  CallbackConnectedMute m res 
  data  CallbackMutedUnmute m res 
  data  CallbackDisconnected m res 
  data  CallbackDropReceived m res 
  
  data  ResPar m res 

  newCallback :: MkResDef m (ResPar m res) (CallbackIdle m res)
  failed :: RequestDef m CallbackRequestFailed (CallRequestReceived m res) '[CallbackFailed m res]
  ackCallbackRequest :: RequestDef m CallbackAckRequest (CallRequestReceived m res) '[CallbackProgress m res]  
  disconnectedProgress :: RequestDef m NotifyCallbackDisconnected (CallbackProgress m res) '[CallbackDisconnected m res]  
  disconnectedConnected :: RequestDef m NotifyCallbackDisconnected (CallbackConnected m res) '[CallbackDisconnected m res]  
  disconnectedMuted :: RequestDef m NotifyCallbackDisconnected (CallbackMuted m res) '[CallbackDisconnected m res]  
  disconnectedConnected2 :: RequestDef m NotifyCallbackDisconnected (CallbackConnectedMute m res) '[CallbackDisconnected m res]  
  disconnectedMuted2 :: RequestDef m NotifyCallbackDisconnected (CallbackMutedUnmute m res) '[CallbackDisconnected m res]  
  disconnectedDropped :: RequestDef m NotifyCallbackDisconnected (CallbackDropReceived m res) '[CallbackDisconnected m res]  
  ackMuted :: RequestDef m CallbackAckMuted (CallbackConnectedMute m res) '[CallbackMuted m res]   
  ackUnMuted :: RequestDef m CallbackAckUnMuted (CallbackMutedUnmute m res) '[CallbackConnected m res]   
  connected :: RequestDef m NotifyCallbackConnected (CallbackProgress m res) '[CallbackConnected m res]   
  callProgress :: RequestDef m NotifyCallbackProgress (CallbackProgress m res) '[CallbackProgress m res]   
  releaseCallback :: RequestDef m ReleaseCallback (CallbackIdle m res) '[CallbackReleased m res]

  idleTransition :: TransitionDef m (CallbackIdle m res) '[CallRequestReceived m res]
  progressTransition :: TransitionDef m (CallbackProgress m res) '[CallbackDropReceived m res]
  connectedTransition :: TransitionDef m (CallbackConnected m res) '[CallbackConnectedMute m res, CallbackDropReceived m res]
  mutedTransition :: TransitionDef m (CallbackMuted m res) '[CallbackMutedUnmute m res, CallbackDropReceived m res]
  --connected2Transition :: TransitionDef m (CallbackConnectedMute m res) '[CallbackDropReceived m res]
  --muted2Transition :: TransitionDef m (CallbackMutedUnmute m res) '[CallbackDropReceived m res]

  failedTransition :: TransitionDef m (CallbackFailed m res) '[CallbackIdle m res]  
  disconnectTransition :: TransitionDef m (CallbackDisconnected m res) '[CallbackIdle m res]  

  termIdle :: TermDef m (CallbackReleased m res)

  _callbackRequest :: CallRequestReceived m res -> m CallbackRequest
--  
buildRes ''Callback

callbackRequest :: Callback m res => StCallRequestReceived m res name -> m CallbackRequest
callbackRequest (St stData) = _callbackRequest stData
