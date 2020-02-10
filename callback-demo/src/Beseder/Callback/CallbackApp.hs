{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PartialTypeSignatures  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE RebindableSyntax       #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLabels       #-}
{-# LANGUAGE TemplateHaskell        #-}

{-# OPTIONS_GHC -fomit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Beseder.Callback.CallbackApp where

import           Protolude                    hiding (Product, handle, return, gets, lift, liftIO,
                                              (>>), (>>=), forever, until,try,on)
-- import           Beseder.Base.Base
import           Beseder.Base.Common
import           Beseder.Base.ControlData                                               
-- import           Beseder.Misc.Misc  
import           Beseder.Callback.Resources.CallbackRes
import           Beseder.Callback.Resources.OutboundCallRes
import qualified Beseder.Callback.Resources.OutboundCallRes as OutboundCallRes
import           Data.String 
-- import           Beseder.Resources.Timer
import           GHC.Exts (Any)    

type OutboundCallAlive name = 
  Not (name :? IsCallDisconnected :|| name :? IsCallFailed :|| name :? IsCallFailed) 

cbAppData :: (_) => OutboundCallRes.ResPar m callRes -> STransData m NoSplitter _ ()
cbAppData callResPar = do
  nextEv
  cbReq <- opRes #cb callbackRequest
  newRes #inCall callResPar 
  invoke #inCall (MakeCall (remoteNumber cbReq) (localNumber cbReq))
  try @(OutboundCallAlive "inCall") $ do
    try @(Not ("cb" :? IsCallbackDropReceived)) $ do
      assert @("inCall" :? IsCallInitiated :&& "cb" :? IsCallRequestReceived)
      invoke #cb CallbackAckRequest
      try @(Not ("inCall" :? IsCallConnected)) $ handleEvents $ do 
        on @("inCall" :? IsCallReachedDest) $ do
          invoke #cb (NotifyCallbackProgress RingbackLocal) 
      newRes #outCall callResPar 
      invoke #outCall (MakeCall (localNumber cbReq) (remoteNumber cbReq))
      try @(OutboundCallAlive "outCall") $ do
        invoke #cb (NotifyCallbackProgress DialingRemote) 
        try @(Not ("outCall" :? IsCallConnected)) $ handleEvents $ do 
          on @("outCall" :? IsCallReachedDest) $ do
            invoke #cb (NotifyCallbackProgress RingbackRemote)
            invoke #inCall PlayRingback 
        on @("inCall" :? IsCallPlayingRingback) (invoke #inCall StopRingback)    
        invoke #cb (NotifyCallbackConnected)
        let bh :: Int
            bh = 0
        invoke #inCall (BridgeCall bh)    
        invoke #outCall (BridgeCall bh)    
        label #callsBridged
        handleEvents $ do 
          caseOf $ do 
            on @("cb" :? IsCallbackConnectedMute) $ do
              invoke #inCall MuteCall
              invoke #cb CallbackAckMuted
            on @("cb" :? IsCallbackMutedUnmute) $ do
              invoke #inCall UnMuteCall
              invoke #cb CallbackAckUnMuted
            endCase
  label #failures
  clear #inCall
  on @(By "outCall") (clear #outCall)
  label #possibleCbStates
  caseOf $ do
    on @("cb" :? IsCallRequestReceived) $ do
      invoke #cb CallbackRequestFailed
    defCase $ invoke #cb NotifyCallbackDisconnected
  nextEv -- what for CallbackRes idle  
  assert @("cb" :? IsCallbackIdle)
  label #probe  




mkSTransDataTypeAny "cbAppData" "CbFunc"   

type InitState = '[StCallbackIdle IO () "cb"]

-- :kind!  ValidateSteps '["probe"]  CbFunc NoSplitter (InitState)
-- :kind ShowLabel' "probe" CbFunc NoSplitter (InitState)
type CbFuncRes = Eval (CbFunc NoSplitter InitState)
type CbValid = ValidateSteps '["probe"] CbFunc NoSplitter InitState
type CbDiagram = StateDiagramSym CbFunc InitState

-- :kind!  ValidateSteps '[] ATMFuncAny NoSplitter (IdleState IO () () ())
-- :kind!  ValidateSteps '["aboutToReleaseTerminal", "handleCleanup"] ATMFuncAny NoSplitter (IdleState IO () () ())
-- :kind!  CbValid


{-
= '[ '("failures",
       '["cb = CallbackProgress , inCall = CallConnected , outCall = CallFailed",
         "cb = CallbackProgress , inCall = CallConnected , outCall = CallDisconnected",
         "cb = CallbackProgress , inCall = CallPlayingRingback , outCall = CallDisconnected",
         "cb = CallbackConnected , inCall = CallBridged , outCall = CallDisconnected",
         "cb = CallbackMuted , inCall = CallMuted , outCall = CallDisconnected",
         "cb = CallbackDropReceived , inCall = CallInitiated",
         "cb = CallbackDropReceived , inCall = CallReachedDest",
         "cb = CallbackDropReceived , inCall = CallConnected , outCall = CallInitiated",
         "cb = CallbackDropReceived , inCall = CallPlayingRingback , outCall = CallReachedDest",
         "cb = CallbackDropReceived , inCall = CallBridged , outCall = CallBridged",
         "cb = CallbackDropReceived , inCall = CallMuted , outCall = CallBridged",
         "cb = CallRequestReceived , inCall = CallFailed",
         "cb = CallbackProgress , inCall = CallDisconnected",
         "cb = CallbackProgress , inCall = CallDisconnected , outCall = CallInitiated",
         "cb = CallbackProgress , inCall = CallDisconnected , outCall = CallReachedDest",
         "cb = CallbackConnected , inCall = CallDisconnected , outCall = CallBridged",
         "cb = CallbackMuted , inCall = CallDisconnected , outCall = CallBridged"])]
-}