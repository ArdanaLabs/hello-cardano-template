module Main (main) where

import Apropos.Plutus.HelloValidator qualified as HelloValidator

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
      HelloValidator.spec
