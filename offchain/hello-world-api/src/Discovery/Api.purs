module HelloWorld.Discovery.Api
  ( mintNft
  -- testing exports
  , seedTx
  , makeNftPolicy
  ) where

import Contract.Prelude

import CBOR as CBOR
import Contract.ScriptLookups as Lookups
import Contract.TxConstraints as Constraints
import Data.BigInt as BigInt
import Plutus.Types.Value as Value

import Contract.Address (getWalletAddress)
import Contract.Aeson (decodeAeson, fromString)
import Contract.Log (logDebug', logInfo')
import Contract.Monad (Contract, liftContractM, liftContractAffM)
import Contract.Scripts (applyArgsM)
import Contract.Transaction (TransactionInput)
import Contract.TxConstraints (TxConstraints)
import Contract.Value (adaToken, scriptCurrencySymbol)
import Data.Array (head)
import Data.Map (keys)
import Data.Set (toUnfoldable)
import Plutus.Types.CurrencySymbol (CurrencySymbol)
import ToData (toData)
import Types.PlutusData (PlutusData)
import Types.Scripts (MintingPolicy)
import Util (buildBalanceSignAndSubmitTx, getUtxos, waitForTx, maxWait)

mintNft :: Contract () CurrencySymbol
mintNft = do
  logInfo' "starting mint"
  txOut <- seedTx
  logDebug' $ "seed tx id was:" <> show txOut
  nftPolicy <- makeNftPolicy txOut
  cs <- liftContractAffM "failed to hash MintingPolicy into CurrencySymbol" $ scriptCurrencySymbol nftPolicy
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
  paramNft <- liftContractM "nft decode failed"
    $ CBOR.nft
    # fromString
    # decodeAeson
    # hush
    # map wrap
  logInfo' "got cbor"
  liftContractM "apply args failed" =<< applyArgsM paramNft [ toData txOut ]

seedTx :: Contract () TransactionInput
seedTx = do
  adr <- liftContractM "no wallet" =<< getWalletAddress
  utxos <- getUtxos adr
  liftContractM "no utxos" $ head $ toUnfoldable $ keys utxos
