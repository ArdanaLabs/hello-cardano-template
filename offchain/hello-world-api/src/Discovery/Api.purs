module Discovery.HelloWorld.Api
  ( mintNft
  ) where

import Contract.Prelude

import CBOR as CBOR
import Contract.Address (getWalletCollateral)
import Contract.Aeson (decodeAeson, fromString)
import Contract.Monad (Contract, liftContractM, liftContractAffM)
import Contract.Prim.ByteArray (hexToByteArray)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (applyArgsM)
import Contract.Transaction (TransactionHash, TransactionInput)
import Contract.TxConstraints (TxConstraints)
import Contract.TxConstraints as Constraints
import Contract.Value (mkTokenName, scriptCurrencySymbol)
import Data.Array (head)
import Data.BigInt as BigInt
import Plutus.Types.CurrencySymbol (CurrencySymbol)
import Plutus.Types.Value (singleton) as Value
import ToData (toData)
import Types.PlutusData (PlutusData)
import Types.Scripts (MintingPolicy)
import Util (buildBalanceSignAndSubmitTx)
import Data.Tuple.Nested((/\),type (/\))

mintNft :: Contract () (CurrencySymbol /\ TransactionHash)
mintNft = do
  collatoral <- liftContractM "no collatoral found" =<< getWalletCollateral
  (txOut :: TransactionInput) <- liftContractM "collatoral empty" $ unwrap >>> _.input <$> head collatoral
  paramNft <- liftContractM "nft decode failed" maybeParamNft
  nftPolicy :: MintingPolicy <- liftContractM "apply args failed"
    =<< applyArgsM paramNft [ toData txOut ]
  cs <- liftContractAffM "hash failed" $ scriptCurrencySymbol nftPolicy
  tn <- liftContractM "token name failed to decode" $ mkTokenName =<< hexToByteArray "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
  let
    lookups :: Lookups.ScriptLookups PlutusData
    lookups = mempty
    constraints :: TxConstraints Unit Unit
    constraints = Constraints.mustMintValue (Value.singleton cs tn (BigInt.fromInt 1))
  txId <- buildBalanceSignAndSubmitTx lookups constraints
  pure $ cs /\ txId

maybeParamNft :: Maybe MintingPolicy
maybeParamNft =
    CBOR.nft
      # fromString
      # decodeAeson
      # hush
      # map wrap

