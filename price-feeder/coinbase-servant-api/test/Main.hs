module Main (main) where

import Test.Syd

import Network.Coinbase.APISpec qualified (spec)

main :: IO ()
main = sydTest $ do
  Network.Coinbase.APISpec.spec
