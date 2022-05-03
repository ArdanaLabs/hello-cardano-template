module Main where

import Test.Syd

import qualified Network.Binance.ClientSpec (spec)

main :: IO ()
main = sydTest $ do
  Network.Binance.ClientSpec.spec
