module Main (main) where

import Test.Syd

import qualified Network.Huobi.APISpec (spec)

main :: IO ()
main = sydTest $ do
  Network.Huobi.APISpec.spec
