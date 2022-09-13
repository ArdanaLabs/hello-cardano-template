module HelloWorld.Discovery.Api
  ( mintNft
  , protocolInit
  , openVault
  , getAllVaults
  , getVault
  , incrementVault
  -- testing exports
  , stealConfig -- TODO move to test module
  , seedTx
  , makeNftPolicy
  ) where

import Contract.Prelude

import CBOR as CBOR
import Contract.Address (getWalletAddress, getWalletCollateral)
import Contract.Hashing (datumHash)
import Contract.Log (logDebug', logError', logInfo')
import Contract.Monad (Contract, liftContractM)
import Contract.PlutusData (Datum(Datum), PlutusData(..), Redeemer(Redeemer), fromData)
import Contract.Prim.ByteArray (hexToByteArray, hexToByteArrayUnsafe)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (Validator(..), applyArgsM, mintingPolicyHash, scriptHashAddress, validatorHash)
import Contract.Transaction (TransactionInput, TransactionOutputWithRefScript)
import Contract.TxConstraints (TxConstraints)
import Contract.TxConstraints as Constraints
import Contract.Value (Value, TokenName, scriptCurrencySymbol, mkTokenName, adaToken, valueOf)
import Contract.Value as Value
import Data.Array (head)
import Data.BigInt as BigInt
import Data.Map (Map, keys)
import Data.Map as Map
import Data.Set (toUnfoldable)
import Debug (traceM)
import Effect.Exception (throw)
import HelloWorld.Discovery.Types (Protocol, VaultId,Vault(Vault), NftRedeemer(NftRedeemer), HelloRedeemer(HelloRedeemer), HelloAction(Inc))
import Plutus.Types.Address (Address(Address))
import Plutus.Types.Credential (Credential(PubKeyCredential))
import Plutus.Types.CurrencySymbol (CurrencySymbol(..), mpsSymbol)
import Plutus.Types.Value (symbols)
import Test.Spec.Assertions (shouldEqual)
import ToData (toData)
import Types.PlutusData (PlutusData)
import Types.Scripts (MintingPolicy)
import Util (buildBalanceSignAndSubmitTx, decodeCbor, decodeCborMp, getDatum, getUtxos, maxWait, waitForTx)

getAllVaults :: Protocol -> Contract () (Map TransactionInput TransactionOutputWithRefScript)
getAllVaults protocol =
  getUtxos (scriptHashAddress $ validatorHash protocol.vaultValidator)
    <#> Map.filter (hasNft protocol)

getVault :: Protocol -> VaultId -> Contract () (TransactionInput /\ TransactionOutputWithRefScript)
getVault protocol token = do
  vaults <- getAllVaults protocol
  cs <- liftContractM "invalid protocol" $ mpsSymbol $ mintingPolicyHash protocol.nftMp
  let valid = Map.filter (\vault -> valueOf (unwrap (unwrap vault).output).amount cs token > BigInt.fromInt 0) vaults
  case Map.toUnfoldable valid of
    [] -> liftEffect $ throw "no vaults"
    [ vault ] -> pure vault
    _ -> liftEffect $ throw "more than one vault of the same token name, this is really bad"

hasNft :: Protocol -> TransactionOutputWithRefScript -> Boolean
hasNft { nftMp } out = case (mpsSymbol $ mintingPolicyHash nftMp) of
  Nothing -> false -- protocol was invalid
  Just cs -> cs `elem` (symbols $ (unwrap (unwrap out).output).amount)

incrementVault :: Protocol -> VaultId -> Contract () Unit
incrementVault protocol vaultId = do
  txin /\ txOut <- getVault protocol vaultId
  utxos <- getUtxos (scriptHashAddress $ validatorHash protocol.vaultValidator)
  (oldVault :: Vault) <- liftContractM "failed to parse old vault" <<< fromData <<< unwrap =<< getDatum (unwrap (unwrap txOut).output).datum
  cs <- liftContractM "invalid protocol" $ mpsSymbol $ mintingPolicyHash protocol.nftMp
  let
    nft :: Value
    nft = Value.singleton cs vaultId $ BigInt.fromInt 1

    red :: Redeemer
    red = Redeemer $ toData $ HelloRedeemer { tn: vaultId ,action: Inc }

    newVault :: Vault
    newVault = Vault { owner: (unwrap oldVault).owner, count: (unwrap oldVault).count + BigInt.fromInt 1 }

    lookups :: Lookups.ScriptLookups PlutusData
    lookups = Lookups.validator protocol.vaultValidator
      <> Lookups.unspentOutputs utxos

    -- TODO emmiting this lookup can cause eronious success
    -- presumably the tx that gets through doesn't do much
    -- but it needs to be looked into and ideally there
    -- should be tests to show it's not dangerous

    constraints :: TxConstraints Unit Unit
    constraints = Constraints.mustSpendScriptOutput txin red
      <> Constraints.mustPayToScript (validatorHash protocol.vaultValidator) (Datum $ newVault # toData) Constraints.DatumInline (enoughForFees <> nft)
      <> Constraints.mustReferenceOutput protocol.config
  txid <- buildBalanceSignAndSubmitTx lookups constraints
  _ <- waitForTx maxWait (scriptHashAddress $ validatorHash protocol.vaultValidator) txid
  pure unit

openVault :: Protocol -> Contract () VaultId
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
    lookups = Lookups.mintingPolicy protocol.nftMp

    constraints :: TxConstraints Unit Unit
    constraints =
      Constraints.mustPayToScript (validatorHash protocol.vaultValidator) (Datum $ vault # toData) Constraints.DatumInline (enoughForFees <> nft)
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
        Constraints.DatumInline
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
    constraints = Constraints.mustMintValue (Value.singleton cs adaToken (BigInt.fromInt 1))
  -- TODO talk to ctl about this again
  -- <> Constraints.mustSpendPubKeyOutput txOut
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
  logInfo' $ "utxos: " <> show utxos
  col <- liftContractM "no collateral" =<< getWalletCollateral
  logInfo' $ "col: " <> show col
  liftContractM "no utxos" $ head $ toUnfoldable $ keys utxos

enoughForFees :: Value.Value
enoughForFees = Value.lovelaceValueOf $ BigInt.fromInt 10_000_000
