module Main where

import Test.Syd

import ClientsSpec qualified (spec)

main :: IO ()
main = sydTest $ do
  ClientsSpec.spec
