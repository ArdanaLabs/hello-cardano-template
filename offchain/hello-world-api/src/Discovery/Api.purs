module HelloWorld.Discovery.Api
  (setConfig
  ,stealConfig
  ) where

import Contract.Prelude

import CBOR as CBOR
import Contract.ScriptLookups as Lookups
import Contract.TxConstraints as Constraints
import Contract.Value as Value
import Data.BigInt as BigInt

import Contract.Aeson (decodeAeson, fromString)
import Contract.Log(logInfo')
import Contract.Monad ( Contract , liftContractM,liftContractAffM)
import Contract.PlutusData (Datum(Datum),Redeemer(Redeemer))
import Contract.Scripts (Validator,validatorHash)
import Contract.Transaction ( TransactionInput)
import Contract.TxConstraints (TxConstraints)
import Data.Time.Duration(Minutes(..))
import ToData(toData)
import Types.PlutusData (PlutusData)
import Util(buildBalanceSignAndSubmitTx,waitForTx,getUtxos)

-- this should later use bytestrings
setConfig :: Int -> Contract () TransactionInput
setConfig n = do
  validator <- liftContractM "decoding failed" maybeConfig
  vhash <- liftContractAffM "Couldn't hash validator" $ validatorHash validator
  let
    lookups :: Lookups.ScriptLookups PlutusData
    lookups = mempty
    constraints :: TxConstraints Unit Unit
    constraints =
      Constraints.mustPayToScript
        vhash
        (Datum $ n
          # BigInt.fromInt
          # toData
        )
        enoughForFees
  txId <- buildBalanceSignAndSubmitTx lookups constraints
  liftContractM "gave up waiting for sendDatumToScript TX" =<< waitForTx waitTime vhash txId

-- This should never work
stealConfig :: TransactionInput -> Contract () Unit
stealConfig input = do
  validator <- liftContractM "decoding failed" maybeConfig
  vhash <- liftContractAffM "Couldn't hash validator" $ validatorHash validator
  utxos <- getUtxos vhash
  let
    lookups :: Lookups.ScriptLookups PlutusData
    lookups = Lookups.validator validator
      <> Lookups.unspentOutputs utxos
    constraints :: TxConstraints Unit Unit
    constraints = Constraints.mustSpendScriptOutput input (Redeemer (toData unit))
  _ <- buildBalanceSignAndSubmitTx lookups constraints
  logInfo' "finished"

maybeConfig :: Maybe Validator
maybeConfig = CBOR.configScript
            # fromString
            # decodeAeson
            # hush
            # map wrap

-- Constants copied from api
enoughForFees :: Value.Value
enoughForFees = Value.lovelaceValueOf $ BigInt.fromInt 10_000_000

waitTime :: Minutes
waitTime = Minutes 5.0
