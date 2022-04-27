-- | Provides the hello world contract
module HelloWorld.Contract (
  HelloWorldSchema,
  contract,
) where


import Control.Monad (forever)
import Data.Text (Text)

import Ledger.Constraints (adjustUnbalancedTx, otherScript, mustPayToTheScript)
import Ledger.Tx (getCardanoTxId)
import Ledger.Ada (adaValueOf)
import Ledger.Typed.Scripts (ValidatorTypes(..))

import Plutus.Contract (
  AsContractError,
  Contract,
  Endpoint,
  type (.\/),
  mkTxConstraints,
  submitUnbalancedTx,
  awaitTxConfirmed,
  logInfo
 )
import Plutus.Contract qualified as PContract

import HelloWorld.ValidatorProxy (helloValidator)

data Hello
instance ValidatorTypes Hello where
  type instance RedeemerType Hello = ()
  type instance DatumType Hello = Integer

-- | REST schema
type HelloWorldSchema =
        Endpoint "initialize" Integer
    .\/ Endpoint "increment" ()
    .\/ Endpoint "read" ()

initialize :: AsContractError e => Integer -> Contract w s e ()
initialize initialInt = do
  let lookups = otherScript helloValidator
      tx = mustPayToTheScript initialInt (adaValueOf 0)
  adjustedTx <- adjustUnbalancedTx <$> mkTxConstraints @Hello lookups tx
  ledgerTx <- submitUnbalancedTx adjustedTx
  awaitTxConfirmed $ getCardanoTxId ledgerTx
  logInfo @String $ "Successfully initialized datum with value: " <> show initialInt

increment :: () -> Contract w s e ()
increment _ = pure ()

readContract :: () -> Contract w s e ()
readContract _ = pure ()

-- | The contract definition
contract :: Contract () HelloWorldSchema Text ()
contract =
  forever $
    PContract.selectList
      [ PContract.endpoint @"initialize" initialize
      , PContract.endpoint @"increment" increment
      , PContract.endpoint @"read" readContract
      ]
