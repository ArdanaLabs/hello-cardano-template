module Test.HelloWorld.Discovery.Api
  ( spec
  ) where

import Contract.Prelude

import Contract.Monad (Contract, ContractEnv(..), liftContractAffM, liftContractM)
import Contract.ScriptLookups as Lookups
import Contract.Test.Plutip (PlutipConfig, withPlutipContractEnv, runContractInEnv)
import Contract.TxConstraints (TxConstraints)
import Contract.TxConstraints as Constraints
import Contract.Value (adaToken, scriptCurrencySymbol)
import Contract.Wallet (withKeyWallet)
import Data.BigInt as BigInt
import Data.UInt as UInt
import HelloWorld.Discovery.Api (mintNft, seedTx, makeNftPolicy)
import Plutus.Types.Value as Value
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (expectError)
import Types.PlutusData (PlutusData)
import Util (buildBalanceSignAndSubmitTx, waitForTx, withOurLogger)

config :: PlutipConfig
config =
  { host: "127.0.0.1"
  , port: UInt.fromInt 8082
  , logLevel: Error
  -- Server configs are used to deploy the corresponding services.
  , ogmiosConfig:
      { port: UInt.fromInt 1338
      , host: "127.0.0.1"
      , secure: false
      , path: Nothing
      }
  , ogmiosDatumCacheConfig:
      { port: UInt.fromInt 10000
      , host: "127.0.0.1"
      , secure: false
      , path: Nothing
      }
  , ctlServerConfig:
      { port: UInt.fromInt 8083
      , host: "127.0.0.1"
      , secure: false
      , path: Nothing
      }
  , postgresConfig:
      { host: "127.0.0.1"
      , port: UInt.fromInt 5433
      , user: "ctxlib"
      , password: "ctxlib"
      , dbname: "ctxlib"
      }
  }

spec :: Spec Unit
spec = do
  describe "HelloWorld.Discovery.Api" $ do
    describe "nft" do
      tryMintNft
      tryDoubleMint

withApiLogger :: ContractEnv () -> ContractEnv ()
withApiLogger = withOurLogger "apiTest.log"

tryMintNft :: Spec Unit
tryMintNft = do
  it "minting an nft should succeed" $ do
    let initialAdaAmount = BigInt.fromInt 20_000_000
    withPlutipContractEnv config [ initialAdaAmount ] \env alice -> do
      runContractInEnv (withApiLogger env) $
        withKeyWallet alice do
          _ <- mintNft
          pure unit

tryDoubleMint :: Spec Unit
tryDoubleMint = do
  it "double minting an nft should not succeed" $ expectError do
    let initialAdaAmount = BigInt.fromInt 20_000_000
    withPlutipContractEnv config [ initialAdaAmount ] \env alice -> do
      runContractInEnv (withApiLogger env) $
        withKeyWallet alice do
          _ <- doubleMint
          pure unit

-- should fail
doubleMint :: Contract () Unit
doubleMint = do
  txOut <- seedTx
  nftPolicy <- makeNftPolicy txOut
  cs <- liftContractAffM "hash failed" $ scriptCurrencySymbol nftPolicy
  let
    lookups :: Lookups.ScriptLookups PlutusData
    lookups = Lookups.mintingPolicy nftPolicy
    constraints :: TxConstraints Unit Unit
    constraints =
      Constraints.mustMintValue (Value.singleton cs adaToken (BigInt.fromInt 1))
  _ <- buildBalanceSignAndSubmitTx lookups constraints
  _ <- buildBalanceSignAndSubmitTx lookups constraints
  pure unit

