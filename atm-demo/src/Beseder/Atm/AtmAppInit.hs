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
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLabels #-}

{-# OPTIONS_GHC -fomit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Beseder.Atm.AtmAppInit where

import           Protolude                    hiding (Product, handle, return, gets, lift, liftIO,
                                              (>>), (>>=), forever, until,try,on, wait)
import           Beseder.Base.ControlData                                               
import           Data.String 

import           Beseder.Base.Common                                               
import           Beseder.Misc.Misc
import           Beseder.SDUI.SDUIContext
import qualified Beseder.Atm.Resources.AccountRes as AccountRes
import           Beseder.Atm.Resources.CashDispenserRes
import           Beseder.Atm.Resources.CardReaderRes
import           Beseder.Atm.Resources.TerminalRes   
import           Beseder.Atm.AtmAppProp
import           SDUI.Data.SDUIData
import           Beseder.SDUI.SDUIResImpl
import           GHC.Exts (Any)    

initATMApp :: 
  forall m dispRes cardRes termRes. 
    Beseder.Atm.Resources.CashDispenserRes.ResPar m dispRes ->
    Beseder.Atm.Resources.CardReaderRes.ResPar m cardRes ->   
    Beseder.Atm.Resources.TerminalRes.ResPar m termRes ->   
    STransData m NoSplitter _ ()
initATMApp dispRes cardRes termRes = do
  newRes #dsp dispRes
  newRes #card cardRes
  newRes #term termRes 


mkSTransDataType "initATMApp" "InitATMApp"   
mkSTransDataTypeAny "initATMApp" "InitATMAppAny"   

type ATMInitResAny = Eval (InitATMAppAny NoSplitter '[()])
type ATMInitRes m dispRes cardRes termRes = Eval ((InitATMApp m dispRes cardRes termRes) NoSplitter '[()])
type ATMInitEdges m dispRes cardRes termRes = Edges (InitATMApp m dispRes cardRes termRes) NoSplitter '[()]
-- :kind! ValidateSteps '[] ATMUIApp NoSplitter '[()]


