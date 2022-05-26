module Main (main) where

import Test.Syd

import Network.Binance.APISpec (spec)

main :: IO ()
main = sydTest $ do
  Network.Binance.APISpec.spec
