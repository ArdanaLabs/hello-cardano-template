module Test.Volume.HelloWorld.Api where

import Prelude

import Contract.Monad (runContractInEnv)
import Contract.Wallet (withKeyWallet)
import Data.Array (replicate)
import Data.BigInt as BigInt
import Data.Foldable (traverse_, fold)
import Data.Tuple (Tuple, fst)
import Data.Tuple.Nested ((/\))
import Data.Typelevel.Undefined (undefined)
import HelloWorld.Api (initialize)
import Plutip.Server (withPlutipContractEnv)
import Test.HelloWorld.EnvRunner (plutipConfig)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Wallet.Key (KeyWallet(..))

spec :: Spec Unit
spec = do
  describe "Volume Test" do
    simpleVolumeTest

simpleVolumeTest :: Spec Unit
simpleVolumeTest = do
  describe "HelloWorld.Api" do
    it "should not fail" $ do
      let distributions = replicate 2 [BigInt.fromInt 20_000_000 ]
      withPlutipContractEnv plutipConfig distributions \env wallets -> do
        traverse_ (\wallet -> runContractInEnv env $ withKeyWallet wallet do void $ initialize 2 2) wallets
        2 `shouldEqual` 2
        -- runContractInEnv env $
        --   withKeyWallet alice do
        --     void $ initialize 10 10

      -- withPlutipContractEnv plutipConfig [ BigInt.fromInt 20, BigInt.fromInt 20] \env (alice /\ bob) -> do
        -- runContractInEnv env $ withKeyWallet (alice) do void $ initialize 2 2
        
    
      