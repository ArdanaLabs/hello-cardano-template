module Util
  (waitForTx
  ,buildBalanceSignAndSubmitTx
  ,getUtxos
  ,getTxScanUrl
  ,ourLogger
  ) where

import Contract.Prelude

import Aeson
  (Aeson
  ,getField
  ,toArray
  ,toObject
  )
import Contract.Address (scriptHashAddress)
import Contract.Log(logInfo',logWarn',logError')
import Contract.Monad (Contract,liftedE)
import Contract.Utxos (UtxoM(UtxoM), utxosAt,getUtxo)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (ValidatorHash)
import Contract.Transaction
  ( TransactionHash(TransactionHash)
  , TransactionOutput
  , TransactionInput(TransactionInput)
  , balanceAndSignTxE
  , submitE
  )
import Contract.TxConstraints (TxConstraints)
import Control.Monad.Error.Class(throwError)
import Data.Array(toUnfoldable,fromFoldable,catMaybes)
import Data.Map (Map)
import Data.Map as Map
import Data.Time.Duration
  (Milliseconds(..)
  ,Seconds(..)
  ,Minutes(..)
  ,class Duration
  ,fromDuration
  ,convertDuration
  ,negateDuration
  )
import Data.List(filterM,List)
import Data.Log.Formatter.Pretty(prettyFormatter)
import Data.Log.Message(Message)
import Effect.Aff (delay)
import Effect.Exception(throw)
import Node.Encoding(Encoding(UTF8))
import Node.FS.Aff(appendTextFile)
import Serialization.Address (NetworkId(TestnetId,MainnetId))
import Types.ByteArray (byteArrayToHex,hexToByteArray)
import Types.PlutusData (PlutusData)
import Types.Transaction(TransactionInput,TransactionHash)

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
              (liftAff <<< delay <<< wrap) (waitTime # fromDuration # unwrap)
              waitForTx (fromDuration d <> fromDuration (negateDuration waitTime)) vhash txid
      Just txin -> do
        logInfo' $ "found tx:" <> show txid
        pure $ Just txin

buildBalanceSignAndSubmitTx
  :: Lookups.ScriptLookups PlutusData
  -> TxConstraints Unit Unit
  -> Contract () TransactionHash
buildBalanceSignAndSubmitTx = buildBalanceSignAndSubmitTx' maxAttempts

maxAttempts :: Int
maxAttempts = 5

buildBalanceSignAndSubmitTx'
  :: Int
  -> Lookups.ScriptLookups PlutusData
  -> TxConstraints Unit Unit
  -> Contract () TransactionHash
buildBalanceSignAndSubmitTx' attempts lookups constraints = do
  ubTx <- liftedE $ Lookups.mkUnbalancedTx lookups constraints
  bsTxE <- balanceAndSignTxE ubTx
  bsTx <- case bsTxE of
    Right bsTx -> pure bsTx
    Left err
      | attempts < maxAttempts -> do
        logError' "Balance failed on retry. This was caused by a UTxO being spent elsewhere. Retries won't work, probably because the UTxO was requested by txid."
        throwError err
      | otherwise -> throwError err
  etxid <- submitE bsTx
  case etxid of
    Right txId -> do
      when (attempts < maxAttempts) $ logWarn' "Retry worked"
      logInfo' $ "Tx ID: " <> show txId
      pure txId
    Left (errs :: Array Aeson) -> do
      logWarn' $ "Possible race condition, attempts remaining: " <> show attempts
      logWarn' $ "submit failed with:" <> show errs
      let (badInputAeson :: Array Aeson) =
            join $ catMaybes $ errs <#> \err -> do
            obj <- toObject err
            badInputs <- hush $ getField obj "badInputs"
            toArray badInputs
      let badInputs =
            catMaybes $ badInputAeson <#> \inputAeson -> do
            obj <- toObject inputAeson
            txIdStr <- hush $getField obj "txId"
            txId <- wrap <$> hexToByteArray txIdStr
            index <- hush $ getField obj "index"
            pure $ TransactionInput {index,transactionId:txId}
      if (length badInputs > 0)
        then do
          logWarn' $ "some inputs were bad:" <> show badInputs
          spent <- waitForSpent badInputs
          if null spent
            then do
              logError' $ "Some inputs were bad but none of the bad inputs were spent."
                <> "\nThis probably wasn't a race condition"
              liftEffect $ throw $ show errs
            else do
              logWarn' $ "Found spent inputs: " <> show spent
          if attempts > 0
            then buildBalanceSignAndSubmitTx' (attempts-1) lookups constraints
            else do
              logError' "Exhausted retries on race conditions"
              liftEffect $ throw $ show errs
        else do
            logError' "No inputs were bad this probably wasn't a race condition"
            liftEffect $ throw $ show errs

waitForSpent :: Array TransactionInput -> Contract () (Array TransactionInput)
waitForSpent inputs = fromFoldable <$> waitForSpent' (Minutes 2.0) (toUnfoldable inputs)

waitForSpent'
  :: forall a.
  Duration a
  => a
  -> List TransactionInput
  -> Contract () (List TransactionInput)
waitForSpent' d inputs = do
    spent <- filterM isSpent inputs
    if null spent && fromDuration d >= (Milliseconds 0.0)
      then do
        (liftAff <<< delay <<< wrap) (waitTime # fromDuration # unwrap)
        logInfo' $ "No spent tx found yet" <> show (convertDuration d :: Seconds)
        waitForSpent' (fromDuration d <> fromDuration (negateDuration waitTime)) inputs
      else
        pure spent

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

isSpent :: TransactionInput -> Contract () Boolean
isSpent input = isNothing <$> getUtxo input

ourLogger :: String -> Message -> Aff Unit
ourLogger path msg = do
  pretty <- prettyFormatter msg
  when (msg.level >= Warn) $ log pretty
  appendTextFile UTF8 path ("\n" <> pretty)

-- The time to wait between ogmios querries when retrying
waitTime :: Seconds
waitTime = Seconds 1.0

