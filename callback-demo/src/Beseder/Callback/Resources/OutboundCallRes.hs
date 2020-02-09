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

module Beseder.Callback.Resources.OutboundCallRes where

import           Protolude    
import           Haskus.Utils.Variant
import           Beseder.Base.Base
import           Beseder.Base.Common
import           Beseder.Resources.ResourceDef

data MakeCall   = MakeCall {src :: Text, dest :: Text} deriving (Eq, Show)
data DropCall   = DropCall deriving (Eq, Show)
data MuteCall   = MuteCall deriving (Eq, Show)
data UnMuteCall  = UnMuteCall deriving (Eq, Show)
data PlayRingback  = PlayRingback deriving (Eq, Show)
data StopRingback  = StopRingback deriving (Eq, Show)
data BridgeCall = BridgeCall {bridgeHandle :: Int} deriving (Eq, Show)  

class Monad m => OutboundCall m res where
  data  CallIdle m res 
  data  CallInitiated m res 
  data  CallFailed m res 
  data  CallReachedDest m res 
  data  CallConnected m res 
  data  CallBridged m res 
  data  CallPlayingRingback m res 
  data  CallMuted m res 
  data  CallDisconnected m res 

  data  ResPar m res 

  newOutboundCall :: MkResDef m (ResPar m res) (CallIdle m res)
  makeCall :: RequestDef m MakeCall (CallIdle m res) '[CallInitiated m res, CallFailed m res]
  playRingback :: RequestDef m PlayRingback (CallConnected m res) '[CallPlayingRingback m res]  
  stopRingback :: RequestDef m StopRingback (CallPlayingRingback m res) '[CallConnected m res]  
  bridgeCall :: RequestDef m BridgeCall (CallConnected m res) '[CallBridged m res]  
  muteCall :: RequestDef m MuteCall (CallBridged m res) '[CallMuted m res]  
  unmuteCall :: RequestDef m UnMuteCall (CallMuted m res) '[CallBridged m res]  

  dropInitiated :: RequestDef m DropCall (CallInitiated m res) '[CallDisconnected m res]  
  dropReached :: RequestDef m DropCall (CallReachedDest m res) '[CallDisconnected m res]  
  dropConnected :: RequestDef m DropCall (CallConnected m res) '[CallDisconnected m res]  
  dropBridged :: RequestDef m DropCall (CallBridged m res) '[CallDisconnected m res]  
  dropMuted :: RequestDef m DropCall (CallMuted m res) '[CallDisconnected m res]  
  dropPlayingRingback :: RequestDef m DropCall (CallPlayingRingback m res) '[CallDisconnected m res]  

  initiatedTransition :: TransitionDef m (CallInitiated m res) '[CallReachedDest m res, CallConnected m res, CallDisconnected m res]
  reachedTransition :: TransitionDef m (CallReachedDest m res) '[CallConnected m res, CallDisconnected m res]
  connectedTransition :: TransitionDef m (CallConnected m res) '[CallDisconnected m res]
  ringbackTransition :: TransitionDef m (CallPlayingRingback m res) '[CallDisconnected m res]
  mutedTransition :: TransitionDef m (CallMuted m res) '[CallDisconnected m res]  
  bridgedTransition :: TransitionDef m (CallBridged m res) '[CallDisconnected m res]  

  termIdle :: TermDef m (CallIdle m res)
  termDisconnected :: TermDef m (CallDisconnected m res)
  termFailed :: TermDef m (CallFailed m res)

--  
buildRes ''OutboundCall

