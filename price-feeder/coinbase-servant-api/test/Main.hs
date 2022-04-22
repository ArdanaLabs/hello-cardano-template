module Main (main) where

import Test.Syd

import qualified Network.Coinbase.APISpec (spec)

main :: IO ()
main = sydTest $ do
  Network.Coinbase.APISpec.spec
