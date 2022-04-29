-- | Provides the hello world contract
module HelloWorld.Contract (
  InitHelloWorldSchema
, IncHelloWorldSchema
, ReadHelloWorldSchema
, initialize
, initializeHandler
, increment
, read'
) where

import Control.Monad (forever)
import Data.Map qualified as Map
import Data.Monoid (Last(..))
import Data.Text (Text)
import Data.Void (Void)

import Ledger (Datum (..))
import Ledger.Ada (adaValueOf)
import Ledger.Constraints (adjustUnbalancedTx, mustPayToOtherScript, otherScript, unspentOutputs)
import Ledger.Tx (ChainIndexTxOut (..), getCardanoTxId)

import Plutus.Contract (
  AsContractError,
  Contract,
  Endpoint,
  awaitTxConfirmed,
  datumFromHash,
  logInfo,
  mkTxConstraints,
  submitUnbalancedTx,
  utxosAt,
  handleError,
  logError,
  awaitPromise,
  endpoint,
  tell
 )
import PlutusTx (FromData, fromBuiltinData)
import PlutusTx.Builtins (mkI)

import HelloWorld.ValidatorProxy (helloValidator, helloValidatorAddress, helloValidatorHash)

-- | REST schema
type InitHelloWorldSchema = Endpoint "initialize" Integer
type IncHelloWorldSchema = Endpoint "increment" ()
type ReadHelloWorldSchema = Endpoint "read" ()

initialize :: Contract (Last Void) InitHelloWorldSchema Text ()
initialize = forever $ handleError (logError @Text) $ awaitPromise $ endpoint @"initialize" initializeHandler

initializeHandler :: Integer -> Contract (Last Void) InitHelloWorldSchema Text ()
initializeHandler initialInt = do
  let lookups = otherScript helloValidator
      tx = mustPayToOtherScript helloValidatorHash (Datum $ mkI initialInt) (adaValueOf 0)
  adjustedTx <- adjustUnbalancedTx <$> mkTxConstraints @Void lookups tx
  ledgerTx <- submitUnbalancedTx adjustedTx
  awaitTxConfirmed $ getCardanoTxId ledgerTx
  logInfo $ "Successfully initialized datum with value: " <> show initialInt

increment :: Contract () IncHelloWorldSchema Text ()
increment = forever $ handleError (logError @Text) $ awaitPromise $ endpoint @"increment" $ const incrementHandler

incrementHandler :: AsContractError e => Contract w s e ()
incrementHandler = do
  (txOutRef, ciTxOut) <- Map.elemAt 0 <$> utxosAt helloValidatorAddress
  maybeHelloWorldDatum <- getDatum' @Integer ciTxOut
  case maybeHelloWorldDatum of
    Nothing ->
      logInfo @Text $ "No hello world datum found at script address, make sure you inserted an initial datum"
    Just oldDatum -> do
      let updatedHelloWorldDatum = oldDatum + 1
          lookups =
            unspentOutputs (Map.singleton txOutRef ciTxOut)
              <> otherScript helloValidator
          tx = mustPayToOtherScript helloValidatorHash (Datum $ mkI updatedHelloWorldDatum) (adaValueOf 0)
      adjustedTx <- adjustUnbalancedTx <$> mkTxConstraints @Void lookups tx
      ledgerTx <- submitUnbalancedTx adjustedTx
      awaitTxConfirmed $ getCardanoTxId ledgerTx
      logInfo $ "Successfully incremented to value " <> show updatedHelloWorldDatum

getDatum' :: (FromData a, AsContractError e) => ChainIndexTxOut -> Contract w s e (Maybe a)
getDatum' (PublicKeyChainIndexTxOut _ _) = return Nothing
getDatum' (ScriptChainIndexTxOut _ _ eitherDatum _) =
  either
    (\datumHash -> (fromBuiltinData =<<) <$> getDatum <$$> datumFromHash datumHash) -- try to get the datum using the datumHash
    (return . fromBuiltinData . getDatum)
    eitherDatum
  where
    f <$$> x = (fmap . fmap) f x

read' :: Contract (Last Integer) ReadHelloWorldSchema Text ()
read' = forever $ handleError (logError @Text) $ awaitPromise $ endpoint @"read" $ const readHandler

readHandler :: AsContractError e => Contract (Last Integer) s e ()
readHandler = do
  (_, ciTxOut) <- Map.elemAt 0 <$> utxosAt helloValidatorAddress
  maybeHelloWorldDatum <- getDatum' @Integer ciTxOut
  case maybeHelloWorldDatum of
    Nothing -> do
      logInfo @Text $ "No hello world datum found at script address, make sure you inserted an initial datum"
      tell $ Last Nothing
    datum -> do
      logInfo  @Text "Found datum"
      tell $ Last datum
