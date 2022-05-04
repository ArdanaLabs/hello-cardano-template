module Main where

import Test.Syd

import qualified Network.Huobi.ClientSpec (spec)

main :: IO ()
main = sydTest $ do
  Network.Huobi.ClientSpec.spec
