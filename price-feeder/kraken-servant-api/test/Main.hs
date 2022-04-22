module Main (main) where

import Test.Syd

import qualified Network.Kraken.APISpec (spec)

main :: IO ()
main = sydTest $ do
  Network.Kraken.APISpec.spec
