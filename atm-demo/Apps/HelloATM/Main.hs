module Main where

import            Protolude    
import            Beseder.SDUI.Env.SDUIEnv
import            Beseder.Atm.AtmAppUIProp

main :: IO ()
main = startHttpApp runATMUI "index.html" 8072 