module Main where

import Test.Syd

import qualified ClientsSpec (spec)

main :: IO ()
main = sydTest $ do
  ClientsSpec.spec
