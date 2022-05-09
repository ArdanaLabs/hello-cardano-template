module Main (main) where

import Test.Syd

import Network.Huobi.APISpec qualified (spec)

main :: IO ()
main = sydTest $ do
  Network.Huobi.APISpec.spec
