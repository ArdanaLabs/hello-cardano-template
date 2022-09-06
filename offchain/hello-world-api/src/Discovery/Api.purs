module HelloWorld.Discovery.Api
  ( mintNft
  , protocolInit
  , openVault
  -- testing exports
  , stealConfig -- TODO move to test module
  , seedTx
  , makeNftPolicy
  ) where

import Contract.Prelude

import CBOR as CBOR
import Contract.ScriptLookups as Lookups
import Contract.TxConstraints as Constraints
import Contract.Value as Value
import Data.BigInt as BigInt

import Contract.Address (getWalletAddress)
import Contract.Hashing (datumHash)
import Contract.Log (logDebug', logInfo')
import Contract.Monad (Contract, liftContractM)
import Contract.Scripts (applyArgsM, validatorHash, mintingPolicyHash, scriptHashAddress)
import Contract.Transaction (TransactionInput)
import Contract.TxConstraints (TxConstraints)
import Contract.PlutusData (Datum(Datum), Redeemer(Redeemer))
import Contract.Value (TokenName, scriptCurrencySymbol, mkTokenName, adaToken)
import Data.Array (head)
import Data.Map (keys)
import Data.Set (toUnfoldable)
import Effect.Exception (throw)
import HelloWorld.Discovery.Types (Protocol, Vault(Vault), NftRedeemer(NftRedeemer))
import Plutus.Types.Address (Address(Address))
import Plutus.Types.CurrencySymbol (CurrencySymbol, mpsSymbol)
import Plutus.Types.Credential (Credential(PubKeyCredential))
import ToData (toData)
import Types.PlutusData (PlutusData)
import Types.Scripts (MintingPolicy)
import Util (buildBalanceSignAndSubmitTx, decodeCbor, decodeCborMp, getUtxos, waitForTx, maxWait)

getAllVaults :: Protocol -> Contract () (Map TransactionInput TransactionOutput)
getAllVaults protocol = getUtxos (scriptHashAddress $ validatorHash protocol.vaultValidator)

openVault :: Protocol -> Contract () TokenName
openVault protocol = do
  nftCs <- liftContractM "mpsSymbol failed" $ mpsSymbol $ mintingPolicyHash protocol.nftMp
  txOut <- seedTx
  nftTn <- liftContractM "failed to make nft token name" $ datumHash (Datum (toData txOut)) <#> unwrap >>= mkTokenName
  let nftRed = NftRedeemer { tn: nftTn, txId: txOut }
  pkh <- getWalletAddress >>= case _ of
    Just (Address { addressCredential: PubKeyCredential pkh }) -> pure pkh
    _ -> liftEffect $ throw "failed to get wallet pubkey hash"
  let
    nft :: Value.Value
    nft = Value.singleton nftCs nftTn $ BigInt.fromInt 1

    vault :: Vault
    vault = Vault { owner: pkh, count: BigInt.fromInt 0 }

    lookups :: Lookups.ScriptLookups PlutusData
    lookups =
      Lookups.mintingPolicy protocol.nftMp
        <> Lookups.validator protocol.vaultValidator

    constraints :: TxConstraints Unit Unit
    constraints =
      Constraints.mustPayToScript (validatorHash protocol.vaultValidator) (Datum $ vault # toData) (enoughForFees <> nft)
        <> Constraints.mustMintValueWithRedeemer (Redeemer $ nftRed # toData) nft
  txid <- buildBalanceSignAndSubmitTx lookups constraints
  _ <- waitForTx maxWait (scriptHashAddress $ validatorHash protocol.vaultValidator) txid
  pure nftTn

-- this should later use bytestrings
protocolInit :: Contract () Protocol
protocolInit = do
  configValidator <- liftContractM "decoding failed" $ decodeCbor CBOR.configScript
  let configVhash = validatorHash configValidator
  cs <- mintNft
  vaultValidatorParam <- liftContractM "decoding failed" $ decodeCbor CBOR.vault
  vaultValidator <- liftContractM "apply args failed" =<< applyArgsM vaultValidatorParam [ toData cs ]
  let vaultValidatorHash = validatorHash vaultValidator
  vaultAuthParam <- liftContractM "decode failed" $ decodeCborMp CBOR.vaultAuthMp
  vaultAuthMp <- liftContractM "apply args failed" =<< applyArgsM vaultAuthParam [ toData $ scriptHashAddress vaultValidatorHash ]
  vaultAuthCs <- liftContractM "mpsSymbol failed" $ mpsSymbol $ mintingPolicyHash vaultAuthMp
  let
    lookups :: Lookups.ScriptLookups PlutusData
    lookups = mempty

    constraints :: TxConstraints Unit Unit
    constraints =
      Constraints.mustPayToScript
        configVhash
        (Datum $ vaultAuthCs # toData)
        (enoughForFees <> Value.singleton cs adaToken (BigInt.fromInt 1))
  txId <- buildBalanceSignAndSubmitTx lookups constraints
  config <- liftContractM "gave up waiting for sendDatumToScript TX" =<< waitForTx maxWait (scriptHashAddress configVhash) txId
  pure $ { config, vaultValidator, nftMp: _ } vaultAuthMp

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
