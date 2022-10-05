module HelloWorld.Api
  ( initialize
  , increment
  , redeem
  , query
  , sendDatumToScript
  , setDatumAtScript
  , redeemFromScript
  , helloScript
  , enoughForFees
  , datumLookup
  , grabFreeAda
  ) where

import Contract.Prelude

import CBOR as CBOR
import Contract.Address (getWalletAddress, ownPaymentPubKeyHash, scriptHashAddress)
import Contract.Log (logInfo', logError', logDebug')
import Contract.Monad (Contract, liftContractM)
import Contract.PlutusData (class ToData, Datum(Datum), PlutusData(..), Redeemer(Redeemer), toData)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (Validator, ValidatorHash, applyArgs, validatorHash)
import Contract.Transaction (TransactionInput, TransactionOutput(..))
import Contract.TxConstraints (TxConstraints)
import Contract.TxConstraints as Constraints
import Contract.Utxos (getUtxo, getWalletBalance)
import Contract.Value (Value)
import Contract.Value as Value
import Data.BigInt as BigInt
import Data.Foldable (for_)
import Data.List ((..), List)
import Data.Map (keys)
import Data.Set as Set
import Data.Time.Duration (Minutes(..))
import Effect.Exception (throw)
import Util (buildBalanceSignAndSubmitTx, waitForTx, getUtxos, decodeCbor, getDatum)

initialize :: Int -> Int -> Contract () TransactionInput
initialize param initialValue = do
  logDebug' $ "initialize hello-world: param: " <> show param <> " initialValue: " <> show initialValue
  validator <- helloScript param
  let vhash = validatorHash validator
  txIn <- sendDatumToScript initialValue vhash
  logDebug' $ "initialized hello-world: param: " <> show param <> " initialValue: " <> show initialValue
  pure txIn

increment :: Int -> TransactionInput -> Contract () TransactionInput
increment param lastOutput = do
  logDebug' $ "increment hello-world: param: " <> show param
  validator <- helloScript param
  let vhash = validatorHash validator
  oldDatum <- datumLookup lastOutput
  let newDatum = oldDatum + param
  txIn <- setDatumAtScript newDatum vhash validator lastOutput
  logDebug' $ "incremented hello-world: param: " <> show param <> " newDatum: " <> show newDatum
  pure txIn

redeem :: Int -> TransactionInput -> Contract () Unit
redeem param lastOutput = do
  logDebug' $ "redeeming hello-world: param: " <> show param
  validator <- helloScript param
  let vhash = validatorHash validator
  redeemFromScript vhash validator lastOutput
  logDebug' $ "redeemed hello-world: param: " <> show param

query :: TransactionInput -> Contract () (Int /\ Value)
query lastOutput = do
  datum <- datumLookup lastOutput
  balance <- getWalletBalance
    >>= liftContractM "Get wallet balance failed"
  pure $ datum /\ balance

waitTime :: Minutes
waitTime = Minutes 5.0

sendDatumToScript :: Int -> ValidatorHash -> Contract () TransactionInput
sendDatumToScript n vhash = do
  let
    lookups :: Lookups.ScriptLookups PlutusData
    lookups = mempty

    constraints :: TxConstraints Unit Unit
    constraints =
      Constraints.mustPayToScript
        vhash
        ( Datum $ n
            # BigInt.fromInt
            # toData
        )
        Constraints.DatumInline
        enoughForFees
  txId <- buildBalanceSignAndSubmitTx lookups constraints
  liftContractM "sendDatumToScript: operation timed out" =<< waitForTx waitTime (scriptHashAddress vhash) txId

setDatumAtScript
  :: Int
  -> ValidatorHash
  -> Validator
  -> TransactionInput
  -> Contract () TransactionInput
setDatumAtScript n vhash validator txInput = do
  utxos <- getUtxos (scriptHashAddress vhash)
  let
    lookups :: Lookups.ScriptLookups PlutusData
    lookups = Lookups.validator validator
      <> Lookups.unspentOutputs utxos

    constraints :: TxConstraints Unit Unit
    constraints =
      (Constraints.mustSpendScriptOutput txInput incRedeemer)
        <>
          ( Constraints.mustPayToScript
              vhash
              ( Datum $ n
                  # BigInt.fromInt
                  # toData
              )
              Constraints.DatumInline
              enoughForFees
          )
  txId <- buildBalanceSignAndSubmitTx lookups constraints
  liftContractM "setDatumAtScript: operation timed out" =<< waitForTx waitTime (scriptHashAddress vhash) txId

redeemFromScript
  :: ValidatorHash
  -> Validator
  -> TransactionInput
  -> Contract () Unit
redeemFromScript vhash validator txInput = do
  utxos <- getUtxos (scriptHashAddress vhash)
  key <- liftContractM "no wallet" =<< ownPaymentPubKeyHash
  let
    lookups :: Lookups.ScriptLookups PlutusData
    lookups = Lookups.validator validator
      <> Lookups.unspentOutputs utxos

    constraints :: TxConstraints Unit Unit
    constraints = Constraints.mustSpendScriptOutput txInput spendRedeemer
      -- TODO
      -- The mustBeSignedBy constraint is a workaround for
      -- https://github.com/Plutonomicon/cardano-transaction-lib/issues/1079
      -- once it's fixed we should remove this
      <> Constraints.mustBeSignedBy key
  txId <- buildBalanceSignAndSubmitTx lookups constraints
  adr <- liftContractM "no wallet" =<< getWalletAddress
  void $ waitForTx waitTime adr txId
  logInfo' "finished"

helloScript :: Int -> Contract () Validator
helloScript n = do
  let
    maybeParamValidator :: Maybe Validator
    maybeParamValidator = decodeCbor CBOR.paramHello
  paramValidator <- liftContractM "Failed to decode the hello-world validator" maybeParamValidator
  -- TODO It'd be cool if this could be an Integer not Data
  applyArgs paramValidator [ Integer $ BigInt.fromInt n ]
    >>= case _ of
      Left err -> do
        logError' $ show err
        liftEffect $ throw $ show err
      Right val -> pure val

datumLookup :: TransactionInput -> Contract () Int
datumLookup lastOutput = do
  TransactionOutput utxo <- getUtxo lastOutput
    >>= liftContractM "couldn't find utxo"
  oldDatum <- getDatum utxo.datum
  asBigInt <- liftContractM "datum wasn't an integer" $ case oldDatum of
    Datum (Integer n) -> Just n
    _ -> Nothing
  liftContractM
    "Datum exceeds maximum size for conversion to 64 bit int"
    -- There's not hard reason not to support this it just doesn't seem worth the refactor
    $ BigInt.toInt asBigInt

grabFreeAda :: Contract () Unit
grabFreeAda = for_ (0 .. 10) grabFreeAdaSingleParam

grabFreeAdaSingleParam :: Int -> Contract () Unit
grabFreeAdaSingleParam n = do
  validator <- helloScript n
  let vhash = validatorHash validator
  utxos <- getUtxos (scriptHashAddress vhash)
  let
    lookups :: Lookups.ScriptLookups PlutusData
    lookups = Lookups.validator validator
      <> Lookups.unspentOutputs utxos

    constraintsList :: List (TxConstraints Unit Unit)
    constraintsList =
      (\input -> Constraints.mustSpendScriptOutput input spendRedeemer)
        <$>
          (Set.toUnfoldable $ keys utxos)
  logDebug' $ "Starting balancing of " <> show n
  traverse_ (buildBalanceSignAndSubmitTx lookups) constraintsList
  logDebug' $ "Finished balancing of " <> show n

data HelloRedemer = Inc | Spend

-- TODO this should probably be generics, but
-- I couldn't get generics to work
instance ToData HelloRedemer where
  toData Inc = Constr (BigInt.fromInt 0) []
  toData Spend = Constr (BigInt.fromInt 1) []

incRedeemer :: Redeemer
incRedeemer = Redeemer (toData Inc)

spendRedeemer :: Redeemer
spendRedeemer = Redeemer (toData Spend)

enoughForFees :: Value.Value
enoughForFees = Value.lovelaceValueOf $ BigInt.fromInt 10_000_000
