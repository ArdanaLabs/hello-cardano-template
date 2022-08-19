module HelloWorld.Discovery.Api
  ( mintNft
  ) where

import Contract.Prelude

import CBOR as CBOR
import Contract.ScriptLookups as Lookups
import Contract.TxConstraints as Constraints
import Data.BigInt as BigInt
import Plutus.Types.Value as Value

import Contract.Address (PaymentPubKeyHash(..), StakePubKeyHash(..), getWalletAddress)
import Contract.Aeson (decodeAeson, fromString)
import Contract.Credential (Credential(..), StakingCredential(..))
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftContractM, liftContractAffM)
import Contract.Prim.ByteArray (hexToByteArray)
import Contract.Scripts (applyArgsM)
import Contract.Transaction (TransactionHash, TransactionInput)
import Contract.TxConstraints (TxConstraints)
import Contract.Value (mkTokenName, scriptCurrencySymbol)
import Data.Time.Duration (Minutes(..))
import Data.Tuple.Nested ((/\), type (/\))
import Plutus.Types.Address (Address(..))
import Plutus.Types.CurrencySymbol (CurrencySymbol)
import ToData (toData)
import Types.PlutusData (PlutusData)
import Types.Scripts (MintingPolicy)
import Util (buildBalanceSignAndSubmitTx, waitForTx)

mintNft :: Contract () (CurrencySymbol /\ TransactionHash)
mintNft = do
  logInfo' $ "started"
  -- Turns out colatoral doesn't work for keywallets with plutip (or maybe in general)
  -- That would probably be a simpler/better way to do this
  txOut <- liftContractM "seed tx failed" =<< seedTx
  logInfo' $ "seed passed: " <> show txOut
  logInfo' "started cbor"
  paramNft <- liftContractM "nft decode failed" maybeParamNft
  logInfo' "got cbor"
  nftPolicy :: MintingPolicy <- liftContractM "apply args failed"
    =<< applyArgsM paramNft [ toData txOut ]
  logInfo' "got mintingPolicy"
  cs <- liftContractAffM "hash failed" $ scriptCurrencySymbol nftPolicy
  logInfo' $ "got cs: " <> show cs
  tn <- liftContractM "token name failed to decode" $ mkTokenName =<< hexToByteArray "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
  logInfo' $ "got tn: " <> show tn
  let
    lookups :: Lookups.ScriptLookups PlutusData
    lookups = Lookups.mintingPolicy nftPolicy
    constraints :: TxConstraints Unit Unit
    constraints =
      -- TODO the error is because I don't set the minting redeemer
      Constraints.mustMintValue (Value.singleton cs tn (BigInt.fromInt 1))
      -- <> Constraints.mustSpendPubKeyOutput txOut
  txId <- buildBalanceSignAndSubmitTx lookups constraints
  pure $ cs /\ txId

seedTx :: Contract () (Maybe TransactionInput)
seedTx = do
  logInfo' "started seedTx"
  adr@(Address{addressCredential,addressStakingCredential})
    <- liftContractM "no wallet" =<< getWalletAddress
  pkh <- liftContractM "wallet was a script" $ case addressCredential of
    PubKeyCredential pkh -> Just pkh
    _ -> Nothing
  mskh <- liftContractM "I think this would mean the staking key was a script? It shouldn't be possible"
      case addressStakingCredential of
              Nothing -> Just Nothing
              (Just (StakingHash (PubKeyCredential skh))) -> Just $ Just skh
              Just _ -> Nothing
  let
    lookups :: Lookups.ScriptLookups PlutusData
    lookups = mempty
    constraints :: TxConstraints Unit Unit
    constraints =
      Constraints.singleton $ Constraints.MustPayToPubKeyAddress (PaymentPubKeyHash pkh) (StakePubKeyHash <$> mskh) Nothing enoughForFees
  th <- buildBalanceSignAndSubmitTx lookups constraints
  waitForTx waitTime adr th


maybeParamNft :: Maybe MintingPolicy
maybeParamNft =
    CBOR.nft
      # fromString
      # decodeAeson
      # hush
      # map wrap

waitTime :: Minutes
waitTime = Minutes 5.0

enoughForFees :: Value.Value
enoughForFees = Value.lovelaceValueOf $ BigInt.fromInt 10_000_000
