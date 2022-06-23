module Main where

import Prelude
import Effect.Console (log)
import Api (enoughForFees)

main :: Effect Unit
main = do
  log $ show enoughForFees 
