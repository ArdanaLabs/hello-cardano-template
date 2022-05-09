module Main (main) where

import Test.Syd

import Network.Kucoin.APISpec qualified (spec)

main :: IO ()
main = sydTest $ do
  Network.Kucoin.APISpec.spec
