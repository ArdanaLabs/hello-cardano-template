module Test.HelloWorld.Discovery.Api
  ( spec
  ) where

import Contract.Prelude

import Contract.Address (getWalletAddress)
import Contract.Log (logInfo')
import Contract.Monad (Contract, ContractEnv(..), liftContractAffM, liftContractM)
import Contract.ScriptLookups as Lookups
import Contract.Test.Plutip (PlutipConfig, withPlutipContractEnv, runContractInEnv)
import Contract.Transaction (TransactionInput(..))
import Contract.TxConstraints (TxConstraints)
import Contract.TxConstraints as Constraints
import Contract.Utxos (getUtxo, getWalletBalance)
import Contract.Value (adaToken, scriptCurrencySymbol)
import Contract.Wallet (withKeyWallet)
import Data.BigInt as BigInt
import Data.Time.Duration (Minutes(..))
import Data.UInt as UInt
import Effect.Exception (throw)
import HelloWorld.Discovery.Api (makeNftPolicy, mintNft, seedTx)
import Plutus.Types.Value as Value
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (expectError, shouldEqual)
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
      txSpent
      mintingWorks
      cantBurn

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
  it "double minting an nft should not succeed" $ do
    let initialAdaAmount = BigInt.fromInt 20_000_000
    withPlutipContractEnv config [ initialAdaAmount ] \env alice -> do
      runContractInEnv (withApiLogger env) $
        withKeyWallet alice do
          doubleMint

txSpent :: Spec Unit
txSpent = do
  it "seedTx is spent after mint" $ do
    let initialAdaAmount = BigInt.fromInt 20_000_000
    withPlutipContractEnv config [ initialAdaAmount ] \env alice -> do
      runContractInEnv (withApiLogger env) $
        withKeyWallet alice do
          checkTxSpent

mintingWorks :: Spec Unit
mintingWorks = do
  it "wallet has nft after mint" $ do
    let initialAdaAmount = BigInt.fromInt 20_000_000
    withPlutipContractEnv config [ initialAdaAmount ] \env alice -> do
      runContractInEnv (withApiLogger env) $
        withKeyWallet alice do
          mintingMints

cantBurn :: Spec Unit
cantBurn = do
  it "burn fails after mint" $ do
    let initialAdaAmount = BigInt.fromInt 20_000_000
    withPlutipContractEnv config [ initialAdaAmount ] \env alice -> do
      runContractInEnv (withApiLogger env) $
        withKeyWallet alice do
          burnFails

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
  expectError $ buildBalanceSignAndSubmitTx lookups constraints

checkTxSpent :: Contract () Unit
checkTxSpent = do
  txOut <- seedTx
  nftPolicy <- makeNftPolicy txOut
  cs <- liftContractAffM "hash failed" $ scriptCurrencySymbol nftPolicy
  logInfo' $ "NFT cs: " <> show cs
  let
    lookups :: Lookups.ScriptLookups PlutusData
    lookups = Lookups.mintingPolicy nftPolicy

    constraints :: TxConstraints Unit Unit
    constraints =
      Constraints.mustMintValue (Value.singleton cs adaToken (BigInt.fromInt 1))
  txId <- buildBalanceSignAndSubmitTx lookups constraints
  adr <- liftContractM "no Address" =<< getWalletAddress
  _ <- liftContractM "wait timed out" =<< waitForTx waitTime adr txId
  getUtxo txOut >>= case _ of
    Nothing -> pure unit
    Just _ -> liftEffect $ throw "seed tx still existed"

mintingMints :: Contract () Unit
mintingMints = do
  cs /\ txid <- mintNft
  adr <- liftContractM "no wallet" =<< getWalletAddress
  _ <- waitForTx waitTime adr txid
  bal <- liftContractM "no ballance" =<< getWalletBalance
  let nfts = Value.valueOf bal cs adaToken
  nfts `shouldEqual` (BigInt.fromInt 1)

burnFails :: Contract () Unit
burnFails = do
  txOut <- seedTx
  nftPolicy <- makeNftPolicy txOut
  cs <- liftContractAffM "hash failed" $ scriptCurrencySymbol nftPolicy
  logInfo' $ "NFT cs: " <> show cs
  let
    mintLookups :: Lookups.ScriptLookups PlutusData
    mintLookups = Lookups.mintingPolicy nftPolicy

    mintConstraints :: TxConstraints Unit Unit
    mintConstraints =
      Constraints.mustMintValue (Value.singleton cs adaToken (BigInt.fromInt 1))
  txid <- buildBalanceSignAndSubmitTx mintLookups mintConstraints
  adr <- liftContractM "no wallet" =<< getWalletAddress
  _ <- waitForTx waitTime adr txid
  let
    burnLookups :: Lookups.ScriptLookups PlutusData
    burnLookups = Lookups.mintingPolicy nftPolicy

    burnConstraints :: TxConstraints Unit Unit
    burnConstraints =
      Constraints.mustMintValue (Value.singleton cs adaToken (BigInt.fromInt $ -1))
  expectError $
    void $ buildBalanceSignAndSubmitTx burnLookups burnConstraints


waitTime :: Minutes
waitTime = 5.0 # Minutes
