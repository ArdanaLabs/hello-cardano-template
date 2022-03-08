module Main ( main ) where

import qualified Models.Vault as Vault (spec)
import Test.Syd (sydTest)

main :: IO ()
main = sydTest $ do
  Vault.spec
