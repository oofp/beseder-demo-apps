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
{-# LANGUAGE OverloadedLabels #-}

{-# OPTIONS_GHC -fomit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Beseder.BlackJack.Apps.BlackJackPromptApp where

import           Protolude                    hiding (Product, handle, return, gets, lift, liftIO,
                                               (>>), (>>=), forever, until,try,on)
import           Beseder.Base.Control                                               
import           Beseder.BlackJack.Resource.BlackJackRes 
import           Beseder.BlackJack.Game.Card
import           Control.Monad.Identity (IdentityT)
import           Data.String 

blackJackPromptApp :: 
  ( MonadIO m
  ) => STransApp IdentityT m NoSplitter '[()] _ _ () -- '[()] '[] ()
blackJackPromptApp = MkApp $ while $ do
  liftIO $ putStrLn ("Shuffling deck" :: Text)
  cardPack <- liftIO $ createCardPack
  newRes #game (newGame cardPack)
  reach @("game" :? IsGameCompleted) $ do
    invoke #game StartBlackJackGame
    try @(By (StPlayerTurn "game")) $ forever $ do 
      cards <- gets #game cardsOfPlayer
      liftIO $ putStrLn (("PlayerCard:" :: Text) <> show cards)
      liftIO $ putStrLn ("Enter h<Enter> to hit or just <Enter> to stand"::Text)
      inp <- liftIO $ getLine
      ifElse (inp == "h")
        (invoke #game PlayerHit)
        (invoke #game PlayerStand)
    invoke #game DealerFirstStep
    forever (invoke #game DealerNextStep)
  invoke #game GetGameOutcome
  res <- gets #game gameOutcome
  liftIO $ putStrLn (("************** Game result:" :: Text) <> (result res))
  liftIO $ putStrLn (("PlayerCard:" :: Text) <> show (playerCards res))
  liftIO $ putStrLn (("DealerCard:" :: Text) <> show (dealerCards res))
  clear #game
  liftIO $ putStrLn ("----------------------------------------------------" :: Text)
  liftIO $ putStrLn ("Enter  y<Enter> to play more or just <Enter> to exit" :: Text)
  more <- liftIO $ getLine
  return (more == "y")




