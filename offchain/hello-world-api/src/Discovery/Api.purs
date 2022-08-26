module HelloWorld.Discovery.Api
  ( mintNft
  , doubleMint
  ) where

import Contract.Prelude

import CBOR as CBOR
import Contract.Address (PaymentPubKeyHash(..), StakePubKeyHash(..), getWalletAddress)
import Contract.Aeson (decodeAeson, fromString)
import Contract.Credential (Credential(..), StakingCredential(..))
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftContractM)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (applyArgsM)
import Contract.Transaction (TransactionHash, TransactionInput)
import Contract.TxConstraints (TxConstraints)
import Contract.TxConstraints as Constraints
import Contract.Value (adaToken, scriptCurrencySymbol)
import Data.BigInt as BigInt
import Data.Time.Duration (Minutes(..))
import Data.Tuple.Nested ((/\), type (/\))
import Effect.Exception (throw)
import Plutus.Types.Address (Address(..))
import Plutus.Types.CurrencySymbol (CurrencySymbol)
import Plutus.Types.Value as Value
import ToData (toData)
import Types.PlutusData (PlutusData)
import Types.Scripts (MintingPolicy)
import Util (buildBalanceSignAndSubmitTx, decodeCbor, decodeCborMp, waitForTx)

mintNft :: Contract () (CurrencySymbol /\ TransactionHash)
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
  pure $ cs /\ txId

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
  paramNft <- liftContractM "decode failed" $ decodeCborMp CBOR.nft
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

waitTime :: Minutes
waitTime = Minutes 5.0

enoughForFees :: Value.Value
enoughForFees = Value.lovelaceValueOf $ BigInt.fromInt 10_000_000
