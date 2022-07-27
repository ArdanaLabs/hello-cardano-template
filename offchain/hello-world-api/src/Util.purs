module Util
  (waitForTx
  ,buildBalanceSignAndSubmitTx
  ,getUtxos
  ,getTxScanUrl
  ) where

import Contract.Prelude

import Contract.Address (scriptHashAddress)
import Contract.Monad
  ( Contract
  , liftedE
  )
import Contract.Log(logInfo')
import Contract.ScriptLookups as Lookups
import Contract.Scripts (ValidatorHash)
import Contract.Transaction
  ( TransactionHash(TransactionHash)
  , TransactionOutput
  , TransactionInput(TransactionInput)
  , balanceAndSignTxE
  , submit
  )
import Contract.TxConstraints (TxConstraints)
import Contract.Utxos (UtxoM(UtxoM), utxosAt)


import Data.Map (Map)
import Data.Map as Map
import Data.Time.Duration
  (Milliseconds(..)
  ,Seconds(..)
  ,class Duration
  ,fromDuration
  ,convertDuration
  ,negateDuration
  )
import Effect.Aff (delay)
import Types.PlutusData (PlutusData)
import Serialization.Address (NetworkId(TestnetId,MainnetId))
import Types.ByteArray (byteArrayToHex)

waitForTx
  :: forall a.
  Duration a
  => a
  -> ValidatorHash
  -> TransactionHash
  -> Contract () (Maybe TransactionInput)
waitForTx d vhash txid = do
  let hasTransactionId :: TransactionInput /\ TransactionOutput -> Boolean
      hasTransactionId (TransactionInput tx /\ _) =
        tx.transactionId == txid
  utxos <- getUtxos vhash
  case fst <$> find hasTransactionId (Map.toUnfoldable utxos :: Array (TransactionInput /\ TransactionOutput)) of
      Nothing ->
        if (fromDuration d <= (Milliseconds 0.0))
          then do
            pure Nothing
          else do
              logInfo' $ "No tx yet, waiting for: " <> show (convertDuration d :: Seconds)
              (liftAff <<< delay <<< wrap) 1000.0
              waitForTx (fromDuration d <> fromDuration (negateDuration (Seconds 1.0))) vhash txid
      Just txin -> do
        logInfo' $ "found tx:" <> show txid
        pure $ Just txin

buildBalanceSignAndSubmitTx
  :: Lookups.ScriptLookups PlutusData
  -> TxConstraints Unit Unit
  -> Contract () TransactionHash
buildBalanceSignAndSubmitTx lookups constraints = do
  ubTx <- liftedE $ Lookups.mkUnbalancedTx lookups constraints
  bsTx <- liftedE $ balanceAndSignTxE ubTx
  txId <- submit bsTx
  logInfo' $ "Tx ID: " <> show txId
  pure txId

getUtxos :: ValidatorHash -> Contract () (Map TransactionInput TransactionOutput)
getUtxos vhash = do
  let scriptAddress = scriptHashAddress vhash
  UtxoM utxos <- fromMaybe (UtxoM Map.empty) <$> utxosAt scriptAddress
  pure utxos

getTxScanUrl :: NetworkId -> TransactionInput -> String
getTxScanUrl TestnetId (TransactionInput {transactionId:TransactionHash hash}) = "https://testnet.cardanoscan.io/transaction/" <> byteArrayToHex hash
getTxScanUrl MainnetId (TransactionInput {transactionId:TransactionHash hash}) = "https://cardanoscan.io/transaction/" <> byteArrayToHex hash
-- The mainnet case isn't tested because it can't be without runing a mainnet transaction
-- but I did find some mainnet transactions and that seems to be the url format

