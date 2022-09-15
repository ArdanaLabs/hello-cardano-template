module Test.HelloWorld.Discovery.Api
  ( spec
  , localOnlySpec
  ) where

import Contract.Prelude

import CBOR as CBOR
import Contract.Address (getWalletAddress, scriptHashAddress)
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftContractM)
import Contract.PlutusData (Datum(..), Redeemer(Redeemer), toData)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (validatorHash)
import Contract.Test.Plutip (runContractInEnv, withPlutipContractEnv)
import Contract.TxConstraints (TxConstraints)
import Contract.TxConstraints as Constraints
import Contract.Utxos (getUtxo, getWalletBalance)
import Contract.Value (adaToken, scriptCurrencySymbol)
import Contract.Wallet (withKeyWallet)
import Data.BigInt as BigInt
import Data.Time.Duration (Minutes(..))
import Effect.Exception (throw)
import HelloWorld.Api (enoughForFees)
import HelloWorld.Discovery.Api (closeVault, getVault, incrementVault, makeNftPolicy, mintNft, openVault, protocolInit, seedTx)
import HelloWorld.Discovery.Types (VaultId, Protocol)
import Node.HTTP.Client (protocol)
import Plutus.Types.Value as Value
import Test.HelloWorld.EnvRunner (EnvRunner, plutipConfig, runEnvSpec)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (expectError, shouldEqual)
import Types.PlutusData (PlutusData)
import Util (buildBalanceSignAndSubmitTx, decodeCbor, getUtxos, maxWait, waitForTx, withOurLogger)

spec :: EnvRunner -> Spec Unit
spec = runEnvSpec do
  describe "HelloWorld.Discovery.Api" do

    describe "misc" do

      when false $ it "script with serialise works" $ useRunnerSimple do
        val <- liftContractM "failed to decode" $ decodeCbor CBOR.trivialSerialise
        let vhash = validatorHash val
        tx1 <- buildBalanceSignAndSubmitTx
          mempty
          (Constraints.mustPayToScript vhash (Datum $ unit # toData) Constraints.DatumInline enoughForFees)
        tx1' <- liftContractM "time out" =<< waitForTx maxWait (scriptHashAddress vhash) tx1
        utxos <- getUtxos (scriptHashAddress vhash)
        buildBalanceSignAndSubmitTx
          (Lookups.validator val <> Lookups.unspentOutputs utxos)
          (Constraints.mustSpendScriptOutput tx1' (Redeemer $ unit # toData))

    describe "nft" do

      it "mint runs" $ useRunnerSimple do
        mintNft

      it "double minting fails on second mint" $ useRunnerSimple do
        txOut <- seedTx
        nftPolicy <- makeNftPolicy txOut
        cs <- liftContractM "hash failed" $ scriptCurrencySymbol nftPolicy
        let
          lookups :: Lookups.ScriptLookups PlutusData
          lookups = Lookups.mintingPolicy nftPolicy

          constraints :: TxConstraints Unit Unit
          constraints = Constraints.mustMintValue (Value.singleton cs adaToken (BigInt.fromInt 1))
        -- TODO same as mintNft talk to ctl about this
        -- <> Constraints.mustSpendPubKeyOutput txOut
        txId <- buildBalanceSignAndSubmitTx lookups constraints
        adr <- liftContractM "no wallet" =<< getWalletAddress
        _ <- waitForTx maxWait adr txId
        expectError $ buildBalanceSignAndSubmitTx lookups constraints

      it "seedTx is spent after mint" $ useRunnerSimple do
        txOut <- seedTx
        nftPolicy <- makeNftPolicy txOut
        cs <- liftContractM "hash failed" $ scriptCurrencySymbol nftPolicy
        logInfo' $ "NFT cs: " <> show cs
        let
          lookups :: Lookups.ScriptLookups PlutusData
          lookups = Lookups.mintingPolicy nftPolicy

          constraints :: TxConstraints Unit Unit
          constraints = Constraints.mustMintValue (Value.singleton cs adaToken (BigInt.fromInt 1))
        -- TODO talk to ctl about this too
        -- <> Constraints.mustSpendPubKeyOutput txOut
        txId <- buildBalanceSignAndSubmitTx lookups constraints
        adr <- liftContractM "no Address" =<< getWalletAddress
        _ <- liftContractM "wait timed out" =<< waitForTx waitTime adr txId
        getUtxo txOut >>= case _ of
          Nothing -> pure unit
          Just _ -> liftEffect $ throw "seed tx still existed"

      it "wallet has nft after mint" $ useRunnerSimple do
        cs <- mintNft
        bal <- liftContractM "no ballance" =<< getWalletBalance
        let nfts = Value.valueOf bal cs adaToken
        nfts `shouldEqual` (BigInt.fromInt 1)

      it "burning nft fails" $ useRunnerSimple do
        txOut <- seedTx
        nftPolicy <- makeNftPolicy txOut
        cs <- liftContractM "hash failed" $ scriptCurrencySymbol nftPolicy
        logInfo' $ "NFT cs: " <> show cs
        let
          mintLookups :: Lookups.ScriptLookups PlutusData
          mintLookups = Lookups.mintingPolicy nftPolicy

          mintConstraints :: TxConstraints Unit Unit
          mintConstraints = Constraints.mustMintValue (Value.singleton cs adaToken (BigInt.fromInt 1))
        -- TODO this one too
        -- <> Constraints.mustSpendPubKeyOutput txOut
        txid <- buildBalanceSignAndSubmitTx mintLookups mintConstraints
        adr <- liftContractM "no wallet" =<< getWalletAddress
        _ <- waitForTx waitTime adr txid
        let
          burnLookups :: Lookups.ScriptLookups PlutusData
          burnLookups = Lookups.mintingPolicy nftPolicy

          burnConstraints :: TxConstraints Unit Unit
          burnConstraints =
            Constraints.mustSpendPubKeyOutput txOut
              <> Constraints.mustMintValue (Value.singleton cs adaToken (BigInt.fromInt $ -1))
        expectError
          $ void
          $ buildBalanceSignAndSubmitTx burnLookups burnConstraints

  describe "protocol" do

    it "initialize protocol" $ useRunnerSimple do
      protocolInit

    it "initialize protocol but fail to steal the utxo" $ useRunnerSimple do
      txin <- _.config <$> protocolInit
      validator <- liftContractM "decoding failed" $ decodeCbor CBOR.configScript
      let vhash = validatorHash validator
      utxos <- getUtxos (scriptHashAddress vhash)
      let
        lookups :: Lookups.ScriptLookups PlutusData
        lookups = Lookups.validator validator <> Lookups.unspentOutputs utxos

        constraints :: TxConstraints Unit Unit
        constraints = Constraints.mustSpendScriptOutput txin (Redeemer (toData unit))
      expectError $ buildBalanceSignAndSubmitTx lookups constraints

    it "init protocol and open a vault" $ useRunnerSimple do
      protocol <- protocolInit
      openVault protocol

    it "find a vault" $ useRunnerSimple do
      protocol <- protocolInit
      vault <- openVault protocol
      getVault protocol vault

    it "increment a vault" $ useRunnerSimple do
      protocol <- protocolInit
      vault <- openVault protocol
      incrementVault protocol vault

    it "find after inc" $ useRunnerSimple do
      protocol <- protocolInit
      vault <- openVault protocol
      incrementVault protocol vault
      getVault protocol vault

    it "close a vault" $ useRunnerSimple do
      protocol <- protocolInit
      vault <- openVault protocol
      closeVault protocol vault

    it "close after inc" $ useRunnerSimple do
      protocol <- protocolInit
      vault <- openVault protocol
      incrementVault protocol vault
      closeVault protocol vault

    it "vault gone after close" $ useRunnerSimple do
      protocol <- protocolInit
      vault <- openVault protocol
      closeVault protocol vault
      expectError $ getVault protocol vault

localOnlySpec :: Spec Unit
localOnlySpec = do
  describe "attacks" $
    it "can't inc alices vault" $ useRunnerAttack $ \asAlice asBob -> do
      (protocol /\ aliceVault) <- asAlice do
        protocol <- protocolInit
        vault <- openVault protocol
        pure $ protocol /\ vault
      asBob do
        expectError $ incrementVault protocol aliceVault

useRunnerAttack :: forall a.
  (  (forall b. Contract () b -> Contract () b)
  -> (forall b. Contract () b -> Contract () b)
  -> Contract () a
  ) -> Aff Unit
useRunnerAttack contract = do
  withPlutipContractEnv plutipConfig ([ BigInt.fromInt 40_000_000]  /\ [ BigInt.fromInt 40_000_000 ]) \env (alice /\ bob) -> do
    runContractInEnv (withOurLogger "apiTest.log" env)
      $ void $ contract (withKeyWallet alice) (withKeyWallet bob)

useRunnerSimple :: forall a. Contract () a -> EnvRunner -> Aff Unit
useRunnerSimple contract runner = do
  runner \env alice -> do
    runContractInEnv (withOurLogger "apiTest.log" env)
      $ withKeyWallet alice
      $ void contract

waitTime :: Minutes
waitTime = 5.0 # Minutes
