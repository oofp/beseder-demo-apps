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

module Beseder.Atm.AtmAppUIProp where

import           Protolude                    hiding (Product, handle, return, gets, lift, liftIO,
                                              (>>), (>>=), forever, until,try,on, wait)
import           Beseder.Base.ControlData                                               
import           Data.String 

import           Beseder.Base.Common                                               
import           Beseder.Misc.Misc
import           Beseder.SDUI.SDUIContext
import qualified Beseder.Atm.Resources.AccountRes as AccountRes
import           Beseder.Atm.Resources.UIImpl.TerminalUI
import           Beseder.Atm.Resources.UIImpl.AccountUI
import           Beseder.Atm.Resources.UIImpl.CashDispenserUI
import           Beseder.Atm.Resources.UIImpl.CardReaderUI
import           Beseder.Atm.AtmAppProp
import           SDUI.Data.SDUIData
import           Beseder.SDUI.SDUIResImpl
import           GHC.Exts (Any)    

type IdleStateUI = IdleState TaskQ UICashDisp UICardReader UITerm

accRes :: SDUIContext ->Beseder.Atm.Resources.UIImpl.AccountUI.ResPar TaskQ UIAccount
accRes ctx = MkUIAccount (UIParams ctx (EntryID "acc") (EntryTitle "Account"))

cardRes :: SDUIContext ->Beseder.Atm.Resources.UIImpl.CardReaderUI.ResPar TaskQ UICardReader
cardRes ctx = MkUICard (UIParams ctx (EntryID "card") (EntryTitle "Card Reader"))

dispRes :: SDUIContext -> Beseder.Atm.Resources.UIImpl.CashDispenserUI.ResPar TaskQ UICashDisp
dispRes ctx = MkUICashDisp (UIParams ctx (EntryID "disp") (EntryTitle "Cash Dispenser"))

termRes:: SDUIContext -> Beseder.Atm.Resources.UIImpl.TerminalUI.ResPar TaskQ UITerm
termRes ctx = MkUITerm (UIParams ctx (EntryID "term") (EntryTitle "ATM Terminal"))

initATMUiApp :: SDUIContext -> STransData TaskQ NoSplitter _ ()
initATMUiApp ctx = do
  newRes #dsp (dispRes ctx)
  newRes #card (cardRes ctx)
  newRes #term (termRes ctx)

atmTaskQApp :: AccountRes.ResPar TaskQ UIAccount -> STransData TaskQ NoSplitter _ ()
atmTaskQApp accountRes = atmAppAsrtData accountRes   

atmUIApp :: SDUIContext -> STransData TaskQ NoSplitter _ ()
atmUIApp ctx = do
  initATMUiApp ctx
  while (atmTaskQApp (accRes ctx) >> return True) 
  clear #card
  clear #dsp
  clear #term
  --clearAllResources


mkSTransDataTypeAny "initATMUiApp" "InitATMUiApp"   

type ATMInitRes = Eval (InitATMUiApp NoSplitter '[()])

mkSTransDataTypeAny "atmUIApp" "ATMUIApp"   

type ATMUIRes = Eval (ATMUIApp NoSplitter '[()])
-- :kind! ValidateSteps '[] ATMUIApp NoSplitter '[()]


runATMUI :: SDUIContext -> IO ()
runATMUI ctx = runAsyncData $ atmUIApp ctx
