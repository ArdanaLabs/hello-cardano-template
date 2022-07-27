module HelloWorld.AppM where

import Contract.Prelude

import Api (datumLookup, helloScript, redeemFromScript, sendDatumToScript, setDatumAtScript)
import Contract.Monad (liftContractAffM, liftContractM, runContract)
import Contract.Transaction (TransactionOutput(..))
import Contract.Utxos (getUtxo)
import Data.BigInt (fromInt, toNumber)
import Effect.Aff (Aff, try)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.Store.Monad (class MonadStore, StoreT, getStore, runStoreT, updateStore)
import HelloWorld.Capability.HelloWorldApi (class HelloWorldApi, FundsLocked(..), HelloWorldIncrement(..))
import HelloWorld.Store as S
import Plutus.Types.Value (getLovelace, valueToCoin)
import Safe.Coerce (coerce)
import Scripts (validatorHash)

newtype AppM a = AppM (StoreT S.Action S.Store Aff a)

runAppM :: forall q i o. S.Store -> H.Component q i o AppM -> Aff (H.Component q i o Aff)
runAppM store = runStoreT store S.reduce <<< coerce

derive newtype instance functorAppM :: Functor AppM
derive newtype instance applyAppM :: Apply AppM
derive newtype instance applicativeAppM :: Applicative AppM
derive newtype instance bindAppM :: Bind AppM
derive newtype instance monadAppM :: Monad AppM
derive newtype instance monadEffectAppM :: MonadEffect AppM
derive newtype instance monadAffAppM :: MonadAff AppM
derive newtype instance monadStoreAppM :: MonadStore S.Action S.Store AppM

instance helloWorldApiAppM :: HelloWorldApi AppM where
  lock (HelloWorldIncrement param) initialValue = do
    { contractConfig } <- getStore
    result <- liftAff $ try $ runContract contractConfig $ do
      validator <- helloScript param
      vhash <- liftContractAffM "Couldn't hash validator" $ validatorHash validator
      lastOutput <- sendDatumToScript initialValue vhash
      TransactionOutput utxo <- getUtxo lastOutput >>= liftContractM "couldn't find utxo"
      pure $ (lastOutput /\ (FundsLocked (toNumber ((getLovelace $ valueToCoin utxo.amount) / fromInt 1_000_000))))
    case result of
      Left err -> pure $ Left err
      Right (lastOutput /\ fundsLocked) -> do
        updateStore $ S.SetLastOutput lastOutput
        pure $ Right fundsLocked
  increment (HelloWorldIncrement param) = do
    { contractConfig, lastOutput } <- getStore
    case lastOutput of
      Nothing -> pure $ Right unit
      Just lastOutput' -> do
        result <- liftAff $ try $ runContract contractConfig $ do
          validator <- helloScript param
          vhash <- liftContractAffM "Couldn't hash validator" $ validatorHash validator
          oldDatum <- datumLookup lastOutput'
          setDatumAtScript (oldDatum + param) vhash validator lastOutput'
        case result of
          Left err -> pure $ Left err
          Right lastOutput'' -> do
            updateStore $ S.SetLastOutput lastOutput''
            pure $ Right unit
  redeem (HelloWorldIncrement param) = do
    { contractConfig, lastOutput } <- getStore
    case lastOutput of
      Nothing -> pure $ Right unit
      Just lastOutput' -> do
        result <- liftAff $ try $ ((void <<< _) <<< runContract) contractConfig $ do
          validator <- helloScript param
          vhash <- liftContractAffM "Couldn't hash validator" $ validatorHash validator
          redeemFromScript vhash validator lastOutput'
        case result of
          Left err -> pure $ Left err
          Right _ -> do
            updateStore S.ResetLastOutput
            pure $ Right unit
