module HelloWorld.Discovery.Api
  ( mintNft
  , doubleMint
  ) where

import Contract.Prelude

import CBOR as CBOR
import Data.BigInt as BigInt
import Plutus.Types.Value as Value

import Contract.Address (PaymentPubKeyHash(..), StakePubKeyHash(..), getWalletAddress, getWalletCollateral)
import Contract.Aeson (decodeAeson, fromString)
import Contract.Credential (Credential(..), StakingCredential(..))
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftContractM, liftContractAffM)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (applyArgsM)
import Contract.Transaction (TransactionHash, TransactionInput)
import Contract.TxConstraints (TxConstraints)
import Contract.TxConstraints as Constraints
import Contract.Value (adaToken, scriptCurrencySymbol)
import Data.Array(head)
import Data.Time.Duration (Minutes(..))
import Data.Tuple.Nested ((/\), type (/\))
import Effect.Exception (throw)
import Plutus.Types.Address (Address(..))
import Plutus.Types.CurrencySymbol (CurrencySymbol)
import ToData (toData)
import Types.PlutusData (PlutusData)
import Types.Scripts (MintingPolicy)
import Util (buildBalanceSignAndSubmitTx, waitForTx)

mintNft :: Contract () (CurrencySymbol /\ TransactionHash)
mintNft = do
  txOut <- seedTx
  nftPolicy <- makeNftPolcy txOut
  cs <- liftContractAffM "hash failed" $ scriptCurrencySymbol nftPolicy
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
  txOut <- seedTx
  nftPolicy <- makeNftPolcy txOut
  cs <- liftContractAffM "hash failed" $ scriptCurrencySymbol nftPolicy
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

seedTx :: Contract () TransactionInput
seedTx = getWalletCollateral
        >>= liftContractM "No collateral"
        >>= head >>> liftContractM "Empty collateral"
        <#> unwrap >>> _.input


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
