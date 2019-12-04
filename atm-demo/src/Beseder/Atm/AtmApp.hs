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
{-# LANGUAGE OverloadedLabels #-}
{-# OPTIONS_GHC -fomit-interface-pragmas #-}


module Beseder.Atm.AtmApp where

import           Protolude                    hiding (Product, handle, return, gets, lift, liftIO,
                                              (>>), (>>=), forever, until,try,on)
import           Beseder.Base.ControlData                                               
import           Beseder.Base.Common
import           Beseder.Misc.Misc
import           Beseder.Resources.Timer
import           Data.String 
    
import           Beseder.Atm.Resources.CashDispenserRes
import           Beseder.Atm.Resources.AccountRes
import           Beseder.Atm.Resources.CardReaderRes
        

type IdleState m resDsp resCard = 
  '[( StDispenserIdle m resDsp "dsp", StCardReaderIdle m resCard "card")]


atmAppData :: STransData m NoSplitter _ ()
atmAppData = do
  nextEv
  on @("card" :? IsCardInvalid) $ do 
    invoke #card EjectCard
    nextEv
    
--evalSTransData' :: forall sp m f xs a. STransData m sp f a -> Proxy xs -> Proxy (Eval (f sp xs))
--evalAtm :: forall m resDsp resCard. _
evalAtm = 
  let px :: Proxy (IdleState TaskQ () ())
      px = Proxy
  in evalSTransData' atmAppData px
  
  
--   