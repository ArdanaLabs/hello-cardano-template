module Main (main) where

import Test.Tasty

import qualified HelloWorld.ContractSpec (testTree)


main :: IO ()
main = do
  defaultMain $ testGroup "Emulator Traces" [
      HelloWorld.ContractSpec.testTree
    ]