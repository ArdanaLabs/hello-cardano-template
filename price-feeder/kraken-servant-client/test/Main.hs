module Main where

import Test.Syd

import qualified Network.Kraken.ClientSpec (spec)

main :: IO ()
main = sydTest $ do
  Network.Kraken.ClientSpec.spec
