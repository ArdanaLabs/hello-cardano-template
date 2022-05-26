module Main (main) where

import Test.Syd

import qualified Network.Binance.APISpec (spec)

main :: IO ()
main = sydTest $ do
  Network.Binance.APISpec.spec
