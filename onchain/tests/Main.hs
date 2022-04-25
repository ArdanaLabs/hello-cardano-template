module Main (main) where

import Apropos.Plutus.AssetClass qualified as AssetClass
import Apropos.Plutus.Auction qualified as Auction
import Apropos.Plutus.Hello qualified as Hello
import Apropos.Plutus.Integer qualified as Integer
import Apropos.Plutus.SingletonValue qualified as SingletonValue
import Apropos.Plutus.Value qualified as Value
import Apropos.Plutus.Vault qualified as Vault

import Test.Syd

--import Apropos
--import Apropos.LogicalModel
--import Apropos.Plutus.Vault

-- TODO use sydtest-discover once nix stabalizes a bit more
-- TODO figure out why sydtest breaks the histograms and fix it

main :: IO ()
main =
  --print (length $ solveAll (logic :: Formula VaultProp))
  sydTest $
    describe "plutus" $ do
      AssetClass.spec
      Integer.spec
      SingletonValue.spec
      Value.spec
      Vault.spec
      Auction.spec
      Hello.spec
