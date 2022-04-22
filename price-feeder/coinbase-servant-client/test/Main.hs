module Main where

import Test.Syd

import qualified Network.Coinbase.ClientSpec (spec)

main :: IO ()
main = sydTest $ do
  Network.Coinbase.ClientSpec.spec
