module Main where

import Test.Syd

import qualified Network.Kucoin.ClientSpec (spec)

main :: IO ()
main = sydTest $ do
  Network.Kucoin.ClientSpec.spec
