-- | Provides the hello world contract
module HelloWorld.Contract (
  HelloWorldSchema,
  contract,
) where


import Control.Monad (forever)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Void (Void)

import Ledger (Datum(..))
import Ledger.Ada (adaValueOf)
import Ledger.Constraints (adjustUnbalancedTx, otherScript, unspentOutputs, mustPayToOtherScript)
import Ledger.Tx (ChainIndexTxOut(..), getCardanoTxId)

import Plutus.Contract (
  AsContractError,
  Contract,
  Endpoint,
  type (.\/),
  mkTxConstraints,
  submitUnbalancedTx,
  awaitTxConfirmed,
  logInfo,
  utxosAt,
  datumFromHash
 )
import Plutus.Contract qualified as PContract
import PlutusTx (FromData, fromBuiltinData)
import  PlutusTx.Builtins (mkI)

import HelloWorld.ValidatorProxy (helloValidator, helloValidatorAddress, helloValidatorHash)

-- | REST schema
type HelloWorldSchema =
        Endpoint "initialize" Integer
    .\/ Endpoint "increment" ()
    .\/ Endpoint "read" ()

initialize :: AsContractError e => Integer -> Contract w s e ()
initialize initialInt = do
  let lookups = otherScript helloValidator
      tx = mustPayToOtherScript helloValidatorHash (Datum $ mkI initialInt) (adaValueOf 0)
  adjustedTx <- adjustUnbalancedTx <$> mkTxConstraints @Void lookups tx
  ledgerTx <- submitUnbalancedTx adjustedTx
  awaitTxConfirmed $ getCardanoTxId ledgerTx
  logInfo @String $ "Successfully initialized datum with value: " <> show initialInt

increment :: AsContractError e => Contract w s e ()
increment = do
  (txOutRef, ciTxOut) <- Map.elemAt 0 <$> utxosAt helloValidatorAddress
  maybeHelloWorldDatum <- getDatum' @Integer ciTxOut
  case maybeHelloWorldDatum of
    Nothing -> 
      logInfo @String $ "No hello world datum found at script address, make sure you inserted an initial datum"
    Just oldDatum -> do
      let updatedHelloWorldDatum = oldDatum + 1
          lookups = unspentOutputs (Map.singleton txOutRef ciTxOut) <>
                    otherScript helloValidator
          tx = mustPayToOtherScript helloValidatorHash (Datum $ mkI updatedHelloWorldDatum) (adaValueOf 0)
      adjustedTx <- adjustUnbalancedTx <$> mkTxConstraints @Void lookups tx
      ledgerTx <- submitUnbalancedTx adjustedTx
      awaitTxConfirmed $ getCardanoTxId ledgerTx
      logInfo @String $ "Successfully incremented to value " <> show updatedHelloWorldDatum

getDatum' :: (FromData a, AsContractError e) => ChainIndexTxOut -> Contract w s e (Maybe a)
getDatum' (PublicKeyChainIndexTxOut _ _) = return Nothing
getDatum' (ScriptChainIndexTxOut _ _ eitherDatum _) =
  either (\datumHash -> (fromBuiltinData =<<) <$> getDatum <$$> datumFromHash datumHash) -- try to get the datum using the datumHash
         (return . fromBuiltinData . getDatum) 
         eitherDatum
  where f <$$> x = (fmap . fmap) f x

readContract :: () -> Contract w s e ()
readContract _ = pure ()

-- | The contract definition
contract :: Contract () HelloWorldSchema Text ()
contract =
  forever $
    PContract.selectList
      [ PContract.endpoint @"initialize" initialize
      , PContract.endpoint @"increment" $ const increment
      , PContract.endpoint @"read" readContract
      ]
