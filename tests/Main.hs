module Main (main) where

import qualified Apropos.Plutus.AssetClass as AssetClass
import qualified Apropos.Plutus.Auction as Auction
import qualified Apropos.Plutus.Integer as Integer
import qualified Apropos.Plutus.SingletonValue as SingletonValue
import qualified Apropos.Plutus.Value as Value
import qualified Apropos.Plutus.Vault as Vault

import Test.Syd

-- TODO use sydtest-discover once nix stabalizes a bit more
-- TODO figure out why sydtest breaks the histograms and fix it

main :: IO ()
main = sydTest $
    describe "plutus" $ do
        AssetClass.spec
        Integer.spec
        SingletonValue.spec
        Value.spec
        Vault.spec
        Auction.spec
