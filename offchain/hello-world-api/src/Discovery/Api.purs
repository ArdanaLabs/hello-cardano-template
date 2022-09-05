module HelloWorld.Discovery.Api
  ( mintNft
  , protocolInit
  , stealConfig
  -- testing exports
  , seedTx
  , makeNftPolicy
  ) where

import Contract.Prelude

import CBOR as CBOR
import Contract.ScriptLookups as Lookups
import Contract.TxConstraints as Constraints
import Contract.Value as Value
import Data.BigInt as BigInt

import Cardano.Types.Value(mpsSymbol)
import Contract.Address (getWalletAddress)
import Contract.Log (logDebug', logInfo')
import Contract.Monad (Contract, liftContractM)
import Contract.Scripts (Validator,applyArgsM,validatorHash,mintingPolicyHash,scriptHashAddress)
import Contract.Transaction (TransactionInput)
import Contract.TxConstraints (TxConstraints)
import Contract.PlutusData(Datum(Datum),Redeemer(Redeemer))
import Contract.Value (adaToken, scriptCurrencySymbol)
import Data.Array (head)
import Data.Map (keys)
import Data.Set (toUnfoldable)
import Data.Tuple.Nested ((/\), type (/\))
import Plutus.Types.CurrencySymbol (CurrencySymbol)
import ToData (toData)
import Types.PlutusData (PlutusData)
import Types.Scripts (MintingPolicy)
import Util (buildBalanceSignAndSubmitTx, decodeCbor, decodeCborMp, getUtxos, waitForTx,maxWait)

-- this should later use bytestrings
protocolInit :: Contract () (TransactionInput /\ Validator /\ MintingPolicy)
protocolInit = do
  configValidator <- liftContractM "decoding failed" $ decodeCbor CBOR.configScript
  let configVhash = validatorHash configValidator
  cs <- mintNft
  vaultValidatorParam <- liftContractM "decoding failed" $ decodeCbor CBOR.vault
  vaultValidator <- liftContractM "apply args failed" =<< applyArgsM vaultValidatorParam [toData cs]
  let vaultValidatorHash = validatorHash vaultValidator
  vaultAuthParam <- liftContractM "decode failed" $ decodeCborMp CBOR.vaultAuthMp
  vaultAuthMp <- liftContractM "apply args failed" =<< applyArgsM vaultAuthParam [toData $ vaultValidatorHash ]
  vaultAuthCs <- liftContractM "mpsSymbol failed" $ mpsSymbol $ mintingPolicyHash vaultAuthMp
  let
    lookups :: Lookups.ScriptLookups PlutusData
    lookups = mempty

    constraints :: TxConstraints Unit Unit
    constraints =
      Constraints.mustPayToScript
        configVhash
        ( Datum $ vaultAuthCs
            # toData
        )
        (enoughForFees <> Value.singleton cs adaToken (BigInt.fromInt 1))
  txId <- buildBalanceSignAndSubmitTx lookups constraints
  config <- liftContractM "gave up waiting for sendDatumToScript TX" =<< waitForTx maxWait (scriptHashAddress configVhash) txId
  pure $ config /\ vaultValidator /\ vaultAuthMp

-- This should never work
stealConfig :: TransactionInput -> Contract () Unit
stealConfig input = do
  validator <- liftContractM "decoding failed" $ decodeCbor CBOR.configScript
  let vhash = validatorHash validator
  utxos <- getUtxos (scriptHashAddress vhash)
  let
    lookups :: Lookups.ScriptLookups PlutusData
    lookups = Lookups.validator validator <> Lookups.unspentOutputs utxos

    constraints :: TxConstraints Unit Unit
    constraints = Constraints.mustSpendScriptOutput input (Redeemer (toData unit))
  _ <- buildBalanceSignAndSubmitTx lookups constraints
  logInfo' "finished"

mintNft :: Contract () CurrencySymbol
mintNft = do
  logInfo' "starting mint"
  txOut <- seedTx
  logDebug' $ "seed tx id was:" <> show txOut
  nftPolicy <- makeNftPolicy txOut
  cs <- liftContractM "failed to hash MintingPolicy into CurrencySymbol" $ scriptCurrencySymbol nftPolicy
  logInfo' $ "NFT cs: " <> show cs
  let
    lookups :: Lookups.ScriptLookups PlutusData
    lookups = Lookups.mintingPolicy nftPolicy

    constraints :: TxConstraints Unit Unit
    constraints =
      Constraints.mustSpendPubKeyOutput txOut
        <> Constraints.mustMintValue (Value.singleton cs adaToken (BigInt.fromInt 1))
  logDebug' "about to submit"
  txId <- buildBalanceSignAndSubmitTx lookups constraints
  logDebug' "submited"
  adr <- liftContractM "no wallet" =<< getWalletAddress
  _ <- waitForTx maxWait adr txId
  pure $ cs

-- | Creates the nft mintingPolicy from the transaction input parameter
makeNftPolicy :: TransactionInput -> Contract () MintingPolicy
makeNftPolicy txOut = do
  paramNft <- liftContractM "nft decode failed" $ decodeCborMp CBOR.nft
  logInfo' "got cbor"
  liftContractM "apply args failed" =<< applyArgsM paramNft [ toData txOut ]

seedTx :: Contract () TransactionInput
seedTx = do
  adr <- liftContractM "no wallet" =<< getWalletAddress
  utxos <- getUtxos adr
  liftContractM "no utxos" $ head $ toUnfoldable $ keys utxos

enoughForFees :: Value.Value
enoughForFees = Value.lovelaceValueOf $ BigInt.fromInt 10_000_000
