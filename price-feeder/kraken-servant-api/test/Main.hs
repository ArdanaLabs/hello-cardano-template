module Main (main) where

import Test.Syd

import Network.Kraken.APISpec qualified (spec)

main :: IO ()
main = sydTest $ do
  Network.Kraken.APISpec.spec
