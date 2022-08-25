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
import Util (buildBalanceSignAndSubmitTx, waitForTx, getUtxos)

import Data.BigInt as BigInt
import Data.Time.Duration (Minutes(..))

import Contract.Aeson (decodeAeson, fromString)
import Contract.Log (logInfo', logError')
import Contract.Monad (Contract, liftContractM, liftContractAffM)
import Contract.PlutusData (Datum(Datum), Redeemer(Redeemer), getDatumByHash)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (Validator, ValidatorHash, applyArgs, validatorHash)
import Contract.Transaction (TransactionInput)
import Contract.TxConstraints (TxConstraints)
import Contract.TxConstraints as Constraints
import Contract.Utxos (getUtxo, getWalletBalance)
import Contract.Value as Value
import Effect.Exception (throw)
import Plutus.Types.Transaction (TransactionOutput(TransactionOutput))
import Plutus.Types.Value (Value)
import ToData (class ToData, toData)
import Types.PlutusData (PlutusData(Constr, Integer))
import Data.Map (keys)
import Data.Set as Set
import Data.Foldable (for_)
import Data.List ((..), List)

initialize :: Int -> Int -> Contract () TransactionInput
initialize param initialValue = do
  validator <- helloScript param
  vhash <- liftContractAffM "Couldn't hash validator" $ validatorHash validator
  sendDatumToScript initialValue vhash

increment :: Int -> TransactionInput -> Contract () TransactionInput
increment param lastOutput = do
  validator <- helloScript param
  vhash <- liftContractAffM "Couldn't hash validator" $ validatorHash validator
  oldDatum <- datumLookup lastOutput
  let newDatum = oldDatum + param
  setDatumAtScript newDatum vhash validator lastOutput

redeem :: Int -> TransactionInput -> Contract () Unit
redeem param lastOutput = do
  validator <- helloScript param
  vhash <- liftContractAffM "Couldn't hash validator" $ validatorHash validator
  redeemFromScript vhash validator lastOutput

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
        enoughForFees
  txId <- buildBalanceSignAndSubmitTx lookups constraints
  liftContractM "gave up waiting for sendDatumToScript TX" =<< waitForTx waitTime vhash txId

setDatumAtScript
  :: Int
  -> ValidatorHash
  -> Validator
  -> TransactionInput
  -> Contract () TransactionInput
setDatumAtScript n vhash validator txInput = do
  utxos <- getUtxos vhash
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
              enoughForFees
          )
  txId <- buildBalanceSignAndSubmitTx lookups constraints
  liftContractM "failed waiting for increment" =<< waitForTx waitTime vhash txId

redeemFromScript
  :: ValidatorHash
  -> Validator
  -> TransactionInput
  -> Contract () Unit
redeemFromScript vhash validator txInput = do
  utxos <- getUtxos vhash
  let
    lookups :: Lookups.ScriptLookups PlutusData
    lookups = Lookups.validator validator
      <> Lookups.unspentOutputs utxos

    constraints :: TxConstraints Unit Unit
    constraints = Constraints.mustSpendScriptOutput txInput spendRedeemer
  _ <- buildBalanceSignAndSubmitTx lookups constraints
  logInfo' "finished"

helloScript :: Int -> Contract () Validator
helloScript n = do
  let
    maybeParamValidator :: Maybe Validator
    maybeParamValidator =
      CBOR.paramHello
        # fromString
        # decodeAeson
        # hush
        # map wrap
  paramValidator <- liftContractM "decoding failed" maybeParamValidator
  -- TODO It'd be cool if this could be an Integer not Data
  applyArgs paramValidator [ Integer $ BigInt.fromInt n ]
    >>= case _ of
      Left err -> do
        logError' $ show paramValidator
        liftEffect $ throw $ show err
      Right val -> pure val

datumLookup :: TransactionInput -> Contract () Int
datumLookup lastOutput = do
  TransactionOutput utxo <- getUtxo lastOutput
    >>= liftContractM "couldn't find utxo"
  oldDatum <-
    utxo.dataHash
      # liftContractM "UTxO had no datum hash"
      >>= getDatumByHash
      >>= liftContractM "Couldn't find datum by hash"
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
  vhash <- liftContractAffM "Couldn't hash validator" $ validatorHash validator
  utxos <- getUtxos vhash
  let
    lookups :: Lookups.ScriptLookups PlutusData
    lookups = Lookups.validator validator
      <> Lookups.unspentOutputs utxos

    constraintsList :: List (TxConstraints Unit Unit)
    constraintsList =
      (\input -> Constraints.mustSpendScriptOutput input spendRedeemer)
        <$>
          (Set.toUnfoldable $ keys utxos)
  logInfo' $ "starting balance" <> show n
  traverse_ (buildBalanceSignAndSubmitTx lookups) constraintsList
  logInfo' $ "finished: " <> show n

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
