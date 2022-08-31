module HelloWorld.Discovery.Api
  ( doubleMint
  , mintNft
  , protocolInit
  , stealConfig
  ) where

import Contract.Prelude

import CBOR as CBOR
import Cardano.Types.Value (mpsSymbol)
import Contract.Address (PaymentPubKeyHash(..), StakePubKeyHash(..), getWalletAddress, scriptHashAddress)
import Contract.Credential (Credential(..), StakingCredential(..))
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftContractM)
import Contract.PlutusData (Datum(Datum), Redeemer(Redeemer))
import Contract.ScriptLookups as Lookups
import Contract.Scripts (Validator, validatorHash, applyArgsM)
import Contract.Transaction (TransactionInput)
import Contract.TxConstraints (TxConstraints)
import Contract.TxConstraints as Constraints
import Contract.Value (adaToken, scriptCurrencySymbol)
import Contract.Value as Value
import Data.BigInt as BigInt
import Data.Time.Duration (Minutes(..))
import Data.Tuple.Nested ((/\), type (/\))
import Effect.Exception (throw)
import Plutus.Types.Address (Address(..))
import Plutus.Types.CurrencySymbol (CurrencySymbol)
import Scripts (mintingPolicyHash)
import ToData (toData)
import Types.PlutusData (PlutusData)
import Types.Scripts (MintingPolicy)
import Util (buildBalanceSignAndSubmitTx, decodeCbor, decodeCborMp, getUtxos, waitForTx)

-- this should later use bytestrings
protocolInit :: Contract () (TransactionInput /\ Validator /\ MintingPolicy)
protocolInit = do
  configValidator <- liftContractM "decoding failed" maybeConfig
  let configVhash = validatorHash configValidator
  cs <- fst <$> mintNft
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
  config <- liftContractM "gave up waiting for sendDatumToScript TX" =<< waitForTx waitTime (scriptHashAddress configVhash) txId
  pure $ config /\ vaultValidator /\ vaultAuthMp

-- This should never work
stealConfig :: TransactionInput -> Contract () Unit
stealConfig input = do
  validator <- liftContractM "decoding failed" maybeConfig
  let vhash = validatorHash validator
  utxos <- getUtxos (scriptHashAddress vhash)
  let
    lookups :: Lookups.ScriptLookups PlutusData
    lookups = Lookups.validator validator <> Lookups.unspentOutputs utxos

    constraints :: TxConstraints Unit Unit
    constraints = Constraints.mustSpendScriptOutput input (Redeemer (toData unit))
  _ <- buildBalanceSignAndSubmitTx lookups constraints
  logInfo' "finished"

mintNft :: Contract () (CurrencySymbol /\ TransactionInput)
mintNft = do
  -- Turns out colatoral doesn't work for keywallets with plutip (or maybe in general)
  -- That would probably be a simpler/better way to do this
  txOut <- liftContractM "seed tx failed" =<< seedTx
  nftPolicy <- makeNftPolcy txOut
  cs <- liftContractM "hash failed" $ scriptCurrencySymbol nftPolicy
  logInfo' $ "NFT cs: " <> show cs
  let
    lookups :: Lookups.ScriptLookups PlutusData
    lookups = Lookups.mintingPolicy nftPolicy

    constraints :: TxConstraints Unit Unit
    constraints =
      Constraints.mustMintValue (Value.singleton cs adaToken (BigInt.fromInt 1))
  txId <- buildBalanceSignAndSubmitTx lookups constraints
  adr <- liftContractM "no wallet" =<< getWalletAddress
  txIn <- liftContractM "gave up waiting for sendDatumToScript TX" =<< waitForTx waitTime adr txId
  pure $ cs /\ txIn

-- should fail
doubleMint :: Contract () Unit
doubleMint = do
  txOut <- liftContractM "seed tx failed" =<< seedTx
  nftPolicy <- makeNftPolcy txOut
  cs <- liftContractM "hash failed" $ scriptCurrencySymbol nftPolicy
  let
    lookups :: Lookups.ScriptLookups PlutusData
    lookups = Lookups.mintingPolicy nftPolicy

    constraints :: TxConstraints Unit Unit
    constraints =
      Constraints.mustMintValue (Value.singleton cs adaToken (BigInt.fromInt 1))
  _ <- buildBalanceSignAndSubmitTx lookups constraints
  _ <- buildBalanceSignAndSubmitTx lookups constraints
  pure unit

makeNftPolcy :: TransactionInput -> Contract () MintingPolicy
makeNftPolcy txOut = do
  paramNft <- liftContractM "nft decode failed" maybeParamNft
  logInfo' "got cbor"
  liftContractM "apply args failed" =<< applyArgsM paramNft [ toData txOut ]

seedTx :: Contract () (Maybe TransactionInput)
seedTx = do
  logInfo' "started seedTx"
  adr@(Address { addressCredential, addressStakingCredential }) <- liftContractM "no wallet" =<< getWalletAddress
  pkh <- liftContractM "wallet was a script" $ case addressCredential of
    PubKeyCredential pkh -> Just pkh
    _ -> Nothing
  mskh <- case addressStakingCredential of
    Nothing -> pure Nothing
    Just (StakingHash (PubKeyCredential skh)) -> pure $ Just skh
    Just (StakingHash (ScriptCredential _)) -> liftEffect $ throw "Wallet was a script? Probably not possible"
    Just (StakingPtr _) -> liftEffect $ throw "Wallet staking credential was a staking ptr."
  -- TODO look into if this is possible and if we should support it
  let
    lookups :: Lookups.ScriptLookups PlutusData
    lookups = mempty

    constraints :: TxConstraints Unit Unit
    constraints =
      Constraints.singleton $ Constraints.MustPayToPubKeyAddress (PaymentPubKeyHash pkh) (StakePubKeyHash <$> mskh) Nothing enoughForFees
  th <- buildBalanceSignAndSubmitTx lookups constraints
  waitForTx waitTime adr th

maybeParamNft :: Maybe MintingPolicy
maybeParamNft = decodeCborMp CBOR.nft

maybeConfig :: Maybe Validator
maybeConfig = decodeCbor CBOR.configScript

-- Constants copied from api
enoughForFees :: Value.Value
enoughForFees = Value.lovelaceValueOf $ BigInt.fromInt 10_000_000

waitTime :: Minutes
waitTime = Minutes 5.0
