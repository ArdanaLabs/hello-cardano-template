module Main where

import Test.Syd

import ClientsSpec (spec)

main :: IO ()
main = sydTest $ do
  ClientsSpec.spec
