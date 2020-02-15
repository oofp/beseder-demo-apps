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

module Beseder.Callback.CallbackTimerApp where

import           Protolude                    hiding (Product, handle, return, gets, lift, liftIO,
                                              (>>), (>>=), forever, until,try,on)
--import           Beseder.Base.Base
import           Beseder.Base.Common
import           Beseder.Base.ControlData                                               
-- import           Beseder.Misc.Misc  
import           Beseder.Callback.Resources.CallbackRes
import           Beseder.Callback.Resources.OutboundCallRes
import qualified Beseder.Callback.Resources.OutboundCallRes as OutboundCallRes
import           Data.String 
import           Beseder.Resources.Timer
import           GHC.Exts (Any)    

type OutboundCallAlive name = 
  Not (name :? IsCallDisconnected :|| name :? IsCallFailed :|| name :? IsCallFailed) 

{-
withTimer :: forall name m sp f. Named name -> Int -> STransData m (sp :&& name :? IsTimerArmed) f () -> 
                  STransData m sp (ComposeFunc
                          (NewResFunc TimerRes name m)
                          (ComposeFunc
                              (InvokeAllFunc StartTimer name)
                              (ComposeFunc
                                (EmbedFunc (name :? IsTimerArmed) f) (ClearAllFunc name)))) ()
withTimer timerName timeoutSec sub = do
  newRes timerName TimerRes 
  invoke timerName (StartTimer timeoutSec)
  try @(name :? IsTimerArmed) sub
  clear timerName

withTimerLimit :: forall name m sp f f_c. Named name -> Int -> 
                  STransData m sp f_c () ->
                  STransData m (sp :&& name :? IsTimerArmed) f () -> 
                  STransData m sp 
                    (ScopeResFunc
                        (ComposeFunc
                           (NewResFunc TimerRes name m)
                           (ComposeFunc
                              (InvokeAllFunc StartTimer name)
                              (ComposeFunc
                                 (EmbedFunc (name :? IsTimerArmed) f)
                                 (ComposeFunc
                                    (CaptureFunc (name :? IsTimerTriggered) f_c)
                                    (ClearAllFunc name)))))) ()
withTimerLimit timerName timeoutSec c_sub sub = scopeRes $ do
  newRes timerName TimerRes 
  invoke timerName (StartTimer timeoutSec)
  try @(name :? IsTimerArmed) sub
  on @(name :? IsTimerTriggered) c_sub
  clear timerName
-}

cbAppData :: (_) => OutboundCallRes.ResPar m callRes -> STransData m NoSplitter _ ()
cbAppData callResPar = do
  nextEv 
  cbReq <- opRes #cb callbackRequest
  newRes #inCall callResPar 
  invoke #inCall (MakeCall (remoteNumber cbReq) (localNumber cbReq))
  try @(OutboundCallAlive "inCall") $ do
    try @(Not ("cb" :? IsCallbackDropReceived)) $ do
      lb #inCallSetup $ do
        assert @("inCall" :? IsCallInitiated :&& "cb" :? IsCallRequestReceived)
        invoke #cb CallbackAckRequest
        withTimer #callInTimer 30 (invoke #inCall DropCall) $ do
          try @(Not ("inCall" :? IsCallConnected)) $ handleEvents $ do 
            on @("inCall" :? IsCallReachedDest) $ do
              invoke #cb (NotifyCallbackProgress RingbackLocal) 
      newRes #outCall callResPar 
      invoke #outCall (MakeCall (localNumber cbReq) (remoteNumber cbReq))
      try @(OutboundCallAlive "outCall") $ do
        lb #outCallSetup $ do
          invoke #cb (NotifyCallbackProgress DialingRemote) 
          withTimer #callInTimer 30 (invoke #outCall DropCall) $ do
            try @(Not ("outCall" :? IsCallConnected)) $ handleEvents $ do 
              on @("outCall" :? IsCallReachedDest) $ do
                invoke #cb (NotifyCallbackProgress RingbackRemote)
                invoke #inCall PlayRingback 
                label #whatHere
          on @("inCall" :? IsCallPlayingRingback) (invoke #inCall StopRingback)    
          invoke #cb (NotifyCallbackConnected)
        let bh :: Int
            bh = 0
        invoke #inCall (BridgeCall bh)    
        invoke #outCall (BridgeCall bh)    
        label #callsBridged
        lb #connectedCalls $ do
          withTimer #connectedCallTimer 3600 (invoke #outCall DropCall) $ do
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
  block $ do
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
type CbValid = ValidateSteps '["probe","whatHere"] CbFunc NoSplitter InitState
type CbDiagram = StateDiagramSym CbFunc InitState


-- :kind!  ValidateSteps '[] ATMFuncAny NoSplitter (IdleState IO () () ())
-- :kind!  ValidateSteps '["aboutToReleaseTerminal", "handleCleanup"] ATMFuncAny NoSplitter (IdleState IO () () ())
-- :kind!  CbValid
-- :kind!  CbDiagram

-- ghcid --command "stack ghci ./src/Beseder/Callback/CallbackTimerApp.hs" --test ":kind! CbValid" -W
-- ghcid --command "stack ghci ./src/Beseder/Callback/CallbackTimerApp.hs" --test ":kind! CbValid" -W --allow-eval

type L name = ShowLabel' name CbFunc NoSplitter InitState

---- $> :kind! L "whatHere"

---- $> :kind!  CbValid

