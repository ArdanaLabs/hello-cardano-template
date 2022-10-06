module Test.Volume.HelloWorld.Api
  ( spec
  ) where

import Prelude

import Contract.Monad (runContractInEnv)
import Contract.Test.Plutip (withPlutipContractEnv)
import Contract.Wallet (withKeyWallet)
import Control.Parallel (parTraverse_)
import Data.Array (replicate, zip, (..))
import Data.BigInt as BigInt
import Data.Tuple (Tuple(Tuple))
import HelloWorld.Api (initialize, increment, redeem)
import Test.HelloWorld.EnvRunner (plutipConfig)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldReturn)

spec :: Spec Unit
spec = do
  describe "Volume Test" do
    basicVolumeTest

basicVolumeTest :: Spec Unit
basicVolumeTest = do
  describe "HelloWorld.Api" do
    let numWallets = 10
    it ("should not fail for " <> show numWallets) do
      let distributions = replicate numWallets [ BigInt.fromInt 20_000_000 ]
      withPlutipContractEnv plutipConfig distributions \env wallets -> do
        parTraverse_
          ( \(Tuple wallet param) -> do
              runContractInEnv env $ withKeyWallet wallet do
                res <- initialize param 2
                res2 <- increment param res
                redeem param res2 `shouldReturn` unit
          )
          $ zip wallets (1 .. numWallets)
