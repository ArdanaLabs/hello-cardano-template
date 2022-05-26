module Main (main) where

import Test.Syd

import Network.Kucoin.APISpec (spec)

main :: IO ()
main = sydTest $ do
  Network.Kucoin.APISpec.spec
