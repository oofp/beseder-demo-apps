{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PartialTypeSignatures  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RebindableSyntax       #-}
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

module Beseder.BlackJack.Apps.BlackJackSDUIApp where

import           Protolude                    hiding (Product, handle, return, gets, lift, liftIO,
                                               (>>), (>>=), forever, until,try,on)
import           Beseder.Base.ControlData                                               
-- import           Beseder.Base.Control (STrans)                                               
import           Beseder.Base.Common
import           Beseder.Misc.Misc
import           Beseder.Resources.Timer 
import           Beseder.BlackJack.Resource.BlackJackRes 
import           Beseder.BlackJack.Game.Card
import           Data.String 
import           Beseder.SDUI.SDUIContext
import           SDUI.Data.SDUIData
import           Beseder.SDUI.SDUIRes
import           Beseder.SDUI.SDUIResImpl
import           Beseder.SDUI.SDUIResHelper 
import           GHC.Exts (Any)    

bjUiRes :: forall m. SDUIContext -> Beseder.SDUI.SDUIResImpl.ResPar m UI
bjUiRes ctx = MkUI (UIParams ctx (EntryID "bjUI") (EntryTitle "Black Jack"))

type HS = '[PlayerHit,PlayerStand]

blackJackSDUIApp :: forall m. 
  ( _ -- TaskPoster m
  ) =>  SDUIContext -> STransData m NoSplitter _ () 
blackJackSDUIApp sduiCtx = do 
  newRes #ui (bjUiRes @m sduiCtx) 
  fl18 <- uiBool #ui "Welecome to BlackJack; Are you 18 years old or older?" "Yes" "No"
  iff fl18 $ do
    while $ do
      delay #tmr 2 (uiShowText #ui ("Shuffling deck" :: Text))
      cardPack <- liftIO $ createCardPack
      newRes #game (newGame cardPack)
      try @(Not ("game" :? IsGameCompleted)) $ do
        invoke #game StartBlackJackGame
        try @(By (StPlayerTurn "game")) $ do 
          handleLoop $ do  
            label #playerHandIn
            cards <- gets #game cardsOfPlayer
            hsReq <- getOptionVar #ui (("Your cards:" :: Text) <> show cards) (Proxy @HS)
            invokeVar #game hsReq 
            label #playerHandOut
        label #dealerStep
        cards <- gets #game cardsOfPlayer
        uiUnit #ui (("Your cards:" :: Text) <> show cards) "Let dealer to draw two card"
        invoke #game DealerFirstStep
        forever $ do 
          dlrCards <- gets #game cardsOfDealer
          uiUnit #ui (("Your cards:" :: Text) <> show cards <> "; Dealer cards:" <> show dlrCards) 
                     "Let dealer to draw his next card"
          invoke #game DealerNextStep
      label #gameOver
      invoke #game GetGameOutcome
      res <- gets #game gameOutcome
      let resultStr = ("Game result:" :: Text) <> result res 
                        <> "; Your cards:" <> show (playerCards res) 
                        <> "; Dealer cards:" <> show (dealerCards res) 
      uiUnit #ui resultStr "OK"
      clear #game
      label #whileEnd
      uiBool #ui "Another round?" "Yes" "No"
  uiUnit #ui "Bye-Bye" "Bye"
  invoke #ui ShutdownUI
  clear #ui
  label #cur


mkSTransDataTypeAny "blackJackSDUIApp" "BlackJackApp"
type GetCurState = GetLabel "cur" BlackJackApp
type GetState l = GetLabel l BlackJackApp
type BlackJackRes = Eval (BlackJackApp NoSplitter '[()])
--type BlackJackVal = ValidateSteps '["cur"] BlackJackApp NoSplitter '[()]

runBlackJackApp :: SDUIContext -> IO ()
runBlackJackApp ctx = runAsyncData $ blackJackSDUIApp ctx

--bjHello :: SDUIContext -> STrans (ContT Bool) TaskQ NoSplitter '[()] _ _ _ () 
--bjHello ctx = interpret (blackJackSDUIApp @TaskQ ctx)

