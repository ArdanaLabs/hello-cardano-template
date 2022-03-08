module Main ( main ) where

import qualified Models.Vault as Vault (spec)
import qualified Models.Auction as Auction (spec)
import Test.Syd (sydTest)

-- TODO should we use sydtest-discover?
main :: IO ()
main = sydTest $ do
  Vault.spec
  Auction.spec
