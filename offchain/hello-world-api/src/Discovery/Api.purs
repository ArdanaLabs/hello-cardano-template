module HelloWorld.Discovery.Api
  ( mintNft
  , protocolInit
  , openVault
  , getAllVaults
  , getVault
  , getMyVaults
  , incrementVault
  , closeVault
  -- testing exports
  , seedTx
  , makeNftPolicy
  , openVault'
  , incrementVault'
  ) where

import Contract.Prelude

import CBOR as CBOR
import Contract.Address (getWalletAddress, getWalletCollateral, ownPaymentPubKeyHash, ownPubKeyHash)
import Contract.Hashing (datumHash)
import Contract.Log (logDebug', logInfo')
import Contract.Monad (Contract, liftContractM)
import Contract.PlutusData (Datum(Datum), Redeemer(Redeemer), fromData)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (applyArgsM, mintingPolicyHash, scriptHashAddress, validatorHash)
import Contract.Transaction (TransactionInput, TransactionOutputWithRefScript)
import Contract.TxConstraints (TxConstraints)
import Contract.TxConstraints as Constraints
import Contract.Utxos (getUtxo)
import Contract.Value (Value, scriptCurrencySymbol, mkTokenName, adaToken, valueOf)
import Contract.Value as Value
import Control.Alternative (guard)
import Data.Array (head)
import Data.BigInt as BigInt
import Data.Map (Map, catMaybes, keys)
import Data.Map as Map
import Data.Set (toUnfoldable)
import Effect.Exception (throw)
import HelloWorld.Discovery.Types (HelloAction(..), HelloRedeemer(HelloRedeemer), NftRedeemer(..), Protocol, Vault(Vault), VaultId)
import Node.HTTP.Client (protocol)
import Plutus.Types.Address (Address(Address))
import Plutus.Types.Credential (Credential(PubKeyCredential))
import Plutus.Types.CurrencySymbol (CurrencySymbol, mpsSymbol)
import Plutus.Types.Value (symbols)
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

getMyVaults :: Protocol -> Contract () (Map TransactionInput TransactionOutputWithRefScript)
getMyVaults protocol = do
  all <- getAllVaults protocol
  catMaybes <$> traverse keepMyVaults all

keepMyVaults :: TransactionOutputWithRefScript -> Contract () (Maybe TransactionOutputWithRefScript)
keepMyVaults ref = do
  isMine <- isMyVault ref
  pure $
    if isMine then Just ref
    else Nothing

isMyVault :: TransactionOutputWithRefScript -> Contract () Boolean
isMyVault ref = do
  (vault :: Vault) <- liftContractM "failed to parse vault" <<< fromData <<< unwrap =<< getDatum (unwrap (unwrap ref).output).datum
  pkh <- liftContractM "no wallet" =<< ownPubKeyHash
  pure $ (unwrap vault).owner == pkh

closeVault :: Protocol -> VaultId -> Contract () Unit
closeVault protocol vaultId = do
  txin <- fst <$> getVault protocol vaultId
  utxos <- getUtxos (scriptHashAddress $ validatorHash protocol.vaultValidator)
  cs <- liftContractM "invalid protocol" $ mpsSymbol $ mintingPolicyHash protocol.nftMp
  key <- liftContractM "no wallet" =<< ownPaymentPubKeyHash
  adr <- liftContractM "no wallet" =<< getWalletAddress
  let
    nft :: Value
    nft = Value.singleton cs vaultId $ BigInt.fromInt 1

    nftRed :: NftRedeemer
    nftRed = Burning

    red :: Redeemer
    red = Redeemer $ toData $ HelloRedeemer { tn: vaultId, action: Spend }

    lookups :: Lookups.ScriptLookups PlutusData
    lookups = Lookups.validator protocol.vaultValidator
      <> Lookups.unspentOutputs utxos
      <> Lookups.mintingPolicy protocol.nftMp

    constraints :: TxConstraints Unit Unit
    constraints = Constraints.mustSpendScriptOutput txin red
      <> Constraints.mustReferenceOutput protocol.config
      <> Constraints.mustMintValueWithRedeemer (Redeemer $ nftRed # toData) (Value.negation nft)
      <> Constraints.mustBeSignedBy key
  txid <- buildBalanceSignAndSubmitTx lookups constraints
  _ <- waitForTx maxWait adr txid
  pure unit

incrementVault :: Protocol -> VaultId -> Contract () Unit
incrementVault = incrementVault' 1

incrementVault' :: Int -> Protocol -> VaultId -> Contract () Unit
incrementVault' step protocol vaultId = do
  txin /\ txOut <- getVault protocol vaultId
  utxos <- getUtxos (scriptHashAddress $ validatorHash protocol.vaultValidator)
  (oldVault :: Vault) <- liftContractM "failed to parse old vault" <<< fromData <<< unwrap =<< getDatum (unwrap (unwrap txOut).output).datum
  cs <- liftContractM "invalid protocol" $ mpsSymbol $ mintingPolicyHash protocol.nftMp
  key <- liftContractM "no wallet" =<< ownPaymentPubKeyHash
  let
    nft :: Value
    nft = Value.singleton cs vaultId $ BigInt.fromInt 1

    red :: Redeemer
    red = Redeemer $ toData $ HelloRedeemer { tn: vaultId, action: Inc }

    newVault :: Vault
    newVault = Vault { owner: (unwrap oldVault).owner, count: (unwrap oldVault).count + BigInt.fromInt step }

    lookups :: Lookups.ScriptLookups PlutusData
    lookups = Lookups.validator protocol.vaultValidator
      <> Lookups.unspentOutputs utxos

    constraints :: TxConstraints Unit Unit
    constraints = Constraints.mustSpendScriptOutput txin red
      <> Constraints.mustPayToScript (validatorHash protocol.vaultValidator) (Datum $ newVault # toData) Constraints.DatumInline (enoughForFees <> nft)
      <> Constraints.mustReferenceOutput protocol.config
      <> Constraints.mustBeSignedBy key
  txid <- buildBalanceSignAndSubmitTx lookups constraints
  _ <- waitForTx maxWait (scriptHashAddress $ validatorHash protocol.vaultValidator) txid
  pure unit

openVault :: Protocol -> Contract () VaultId
openVault = openVault' 0

openVault' :: Int -> Protocol -> Contract () VaultId
openVault' start protocol = do
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
    vault = Vault { owner: pkh, count: BigInt.fromInt start }

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

mintNft :: Contract () CurrencySymbol
mintNft = do
  logInfo' "starting mint"
  txOut <- seedTx
  adr <- liftContractM "no wallet" =<< getWalletAddress
  utxos <- getUtxos adr
  logDebug' $ "seed tx id was:" <> show txOut
  nftPolicy <- makeNftPolicy txOut
  cs <- liftContractM "failed to hash MintingPolicy into CurrencySymbol" $ scriptCurrencySymbol nftPolicy
  logInfo' $ "NFT cs: " <> show cs
  let
    lookups :: Lookups.ScriptLookups PlutusData
    lookups = Lookups.mintingPolicy nftPolicy
      <> Lookups.unspentOutputs utxos

    constraints :: TxConstraints Unit Unit
    constraints = Constraints.mustMintValue (Value.singleton cs adaToken (BigInt.fromInt 1))
      <> Constraints.mustSpendPubKeyOutput txOut
  logDebug' "about to submit"
  txId <- buildBalanceSignAndSubmitTx lookups constraints
  logDebug' "submited"
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
  let colIns = (unwrap >>> _.input) <$> col
  logInfo' $ "colIns: " <> show colIns
  let nonColateralUtxOs = Map.filterKeys (\utxo -> utxo `notElem` colIns) utxos
  logInfo' $ "nonColUtxos: " <> show nonColateralUtxOs
  case head $ toUnfoldable $ keys nonColateralUtxOs of
    Just sending -> do
      logInfo' $ "sending: " <> show sending
      out <- liftContractM "no output" =<< getUtxo sending
      logInfo' $ "out: " <> show out
      pure sending
    Nothing -> do
      logInfo' "all utxos were collateral using collateral utxo"
      liftContractM "no utxos at all" $ head $ toUnfoldable $ keys utxos

enoughForFees :: Value.Value
enoughForFees = Value.lovelaceValueOf $ BigInt.fromInt 10_000_000
