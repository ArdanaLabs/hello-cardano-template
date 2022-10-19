module Util
  ( waitForTx
  , buildBalanceSignAndSubmitTx
  , getUtxos
  , getTxScanUrl
  , withOurLogger
  , decodeCbor
  , decodeCborMp
  , maxWait
  , getDatum
  ) where

import Contract.Prelude

import Aeson (Aeson, getField, toArray, toObject)
import Contract.Address (Address, NetworkId(..))
import Contract.Log (logDebug', logError', logInfo', logWarn')
import Contract.Monad (Contract, ContractEnv, liftedE, liftContractM)
import Contract.PlutusData (Datum, PlutusData, getDatumByHash)
import Contract.Prim.ByteArray (byteArrayToHex, hexToByteArray)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (MintingPolicy(..), Validator(..))
import Contract.Transaction (OutputDatum(..), TransactionHash(TransactionHash), TransactionInput(TransactionInput), TransactionOutputWithRefScript, balanceAndSignTxE, plutusV2Script, submitE)
import Contract.TxConstraints (TxConstraints)
import Contract.Utxos (utxosAt, getUtxo)
import Control.Monad.Error.Class (throwError)
import Data.Array (toUnfoldable, fromFoldable, catMaybes)
import Data.List (filterM, List)
import Data.Log.Formatter.Pretty (prettyFormatter)
import Data.Log.Message (Message)
import Data.Map (Map)
import Data.Map as Map
import Data.Time.Duration (Milliseconds(..), Seconds(..), Minutes(..), class Duration, fromDuration, convertDuration, negateDuration)
import Effect.Aff (delay)
import Effect.Aff.Retry (retrying, limitRetries, RetryStatus(RetryStatus))
import Effect.Exception (throw, error)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Aff (appendTextFile)

waitForTx
  :: forall a
   . Duration a
  => a
  -> Address
  -> TransactionHash
  -> Contract () (Maybe TransactionInput)
waitForTx d adr txid = do
  let
    hasTransactionId :: TransactionInput /\ TransactionOutputWithRefScript -> Boolean
    hasTransactionId (TransactionInput tx /\ _) =
      tx.transactionId == txid
  utxos <- getUtxos adr
  case fst <$> find hasTransactionId (Map.toUnfoldable utxos :: Array (TransactionInput /\ TransactionOutputWithRefScript)) of
    Nothing ->
      if (fromDuration d <= (Milliseconds 0.0)) then do
        pure Nothing
      else do
        logInfo' $ "No tx yet, waiting for: " <> show (convertDuration d :: Seconds)
        (liftAff <<< delay <<< wrap) (waitTime # fromDuration # unwrap)
        waitForTx (fromDuration d <> fromDuration (negateDuration waitTime)) adr txid
    Just txin -> do
      logInfo' $ "found tx:" <> show txid
      pure $ Just txin

buildBalanceSignAndSubmitTx
  :: Lookups.ScriptLookups PlutusData
  -> TxConstraints Unit Unit
  -> Contract () TransactionHash
buildBalanceSignAndSubmitTx lookups constraints = liftedE
  $ mapLeft (map $ show >>> error)
  <$>
    retrying
      (limitRetries maxAttempts)
      check
      (tryBuildBalanceSignAndSubmitTx lookups constraints)

maxAttempts :: Int
maxAttempts = 5

tryBuildBalanceSignAndSubmitTx
  :: Lookups.ScriptLookups PlutusData
  -> TxConstraints Unit Unit
  -> RetryStatus
  -> Contract () (Either (Array Aeson) TransactionHash)
tryBuildBalanceSignAndSubmitTx lookups constraints (RetryStatus { iterNumber }) = do
  ubTx <- liftedE $ Lookups.mkUnbalancedTx lookups constraints
  logDebug' $ "ubTx was:" <> show ubTx
  balanceAndSignTxE ubTx >>= case _ of
    Right bsTx -> do
      logDebug' $ "Tx was:" <> show bsTx
      submitE bsTx >>= case _ of
        Left err -> pure $ Left err
        Right txid -> do
          when (iterNumber > 0) $ logWarn' "Successfull retry"
          pure $ Right txid
    Left err -> do
      when (iterNumber > 0) $ logError' "Balance failed on retry. This was caused by a UTxO being spent elsewhere. Retries won't work, probably because the UTxO was requested by txid."
      throwError $ err

check :: RetryStatus -> (Either (Array Aeson) TransactionHash) -> Contract () Boolean
check _ (Right _) = pure false
check (RetryStatus { iterNumber }) (Left errs) = do
  logWarn' $ "Possible race condition. Retry number: " <> show iterNumber
  logWarn' $ "submit failed with:" <> show errs
  let
    badInputs =
      ( ( ( errs <#> \err -> do
              obj <- toObject err
              badInputField <- hush $ getField obj "badInputs"
              toArray badInputField
          ) # catMaybes # join
        )
          <#> \inputAeson -> do
            obj <- toObject inputAeson
            txIdStr <- hush $ getField obj "txId"
            txId <- wrap <$> hexToByteArray txIdStr
            index <- hush $ getField obj "index"
            pure $ TransactionInput { index, transactionId: txId }
      ) # catMaybes
  if null badInputs then do
    logError' "No inputs were bad this probably wasn't a race condition"
    pure false
  else do
    logWarn' $ "some inputs were bad:" <> show badInputs
    spent <- waitForSpent badInputs
    if null spent then do
      logError' $ "Some inputs were bad but none of the bad inputs were spent."
        <> "\nThis probably wasn't a race condition"
      pure false
    else do
      logWarn' $ "Found spent inputs: " <> show spent
      pure true

-- I'm sure this is implemented somewhere but I couldn't find it
mapLeft :: forall a b c. (a -> b) -> Either a c -> Either b c
mapLeft f = either (Left <<< f) Right

waitForSpent :: Array TransactionInput -> Contract () (Array TransactionInput)
waitForSpent inputs = fromFoldable <$> waitForSpent' maxWait (toUnfoldable inputs)

maxWait :: Minutes
maxWait = Minutes 5.0

waitForSpent'
  :: forall a
   . Duration a
  => a
  -> List TransactionInput
  -> Contract () (List TransactionInput)
waitForSpent' d inputs = do
  spent <- filterM isSpent inputs
  if null spent && fromDuration d >= (Milliseconds 0.0) then do
    (liftAff <<< delay <<< wrap) (waitTime # fromDuration # unwrap)
    logInfo' $ "No spent tx found yet" <> show (convertDuration d :: Seconds)
    waitForSpent' (fromDuration d <> fromDuration (negateDuration waitTime)) inputs
  else
    pure spent

getUtxos :: Address -> Contract () (Map TransactionInput TransactionOutputWithRefScript)
getUtxos adr = fromMaybe (Map.empty) <$> utxosAt adr

getTxScanUrl :: NetworkId -> TransactionInput -> String
getTxScanUrl TestnetId (TransactionInput { transactionId: TransactionHash hash }) = "https://testnet.cardanoscan.io/transaction/" <> byteArrayToHex hash
getTxScanUrl MainnetId (TransactionInput { transactionId: TransactionHash hash }) = "https://cardanoscan.io/transaction/" <> byteArrayToHex hash

-- The mainnet case isn't tested because it can't be without runing a mainnet transaction
-- but I did find some mainnet transactions and that seems to be the url format

isSpent :: TransactionInput -> Contract () Boolean
isSpent input = isNothing <$> getUtxo input

withOurLogger :: String -> ContractEnv () -> ContractEnv ()
withOurLogger path env = wrap $ (unwrap env)
  { config
      { customLogger = Just $ ourLogger path
      , logLevel = Warn
      }
  }

getDatum :: OutputDatum -> Contract () Datum
getDatum = case _ of
  NoOutputDatum -> liftEffect $ throw "no output datum"
  OutputDatumHash dh -> getDatumByHash dh >>= liftContractM "Datum hash lookup failed"
  OutputDatum d -> pure d

decodeCbor :: String -> Maybe Validator
decodeCbor cborHex = Validator <<< plutusV2Script <$> hexToByteArray cborHex

decodeCborMp :: String -> Maybe MintingPolicy
decodeCborMp cborHex = MintingPolicy <<< plutusV2Script <$> hexToByteArray cborHex

ourLogger :: String -> Message -> Aff Unit
ourLogger path msg = do
  pretty <- prettyFormatter msg
  when (msg.level >= Warn) $ log pretty
  appendTextFile UTF8 path ("\n" <> pretty)

-- The time to wait between ogmios querries when retrying
waitTime :: Seconds
waitTime = Seconds 1.0

