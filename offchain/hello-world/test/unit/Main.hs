module Main (main) where

import Test.Tasty

import HelloWorld.ContractSpec qualified (testTree)

main :: IO ()
main = do
  defaultMain $
    testGroup
      "Emulator Traces"
      [ HelloWorld.ContractSpec.testTree
      ]
