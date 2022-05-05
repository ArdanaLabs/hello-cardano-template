module Main (main) where

import Test.Syd

import qualified Network.Kucoin.APISpec (spec)

main :: IO ()
main = sydTest $ do
  Network.Kucoin.APISpec.spec
