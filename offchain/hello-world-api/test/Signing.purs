module Test.HelloWorld.Signing
  ( spec
  ) where

import Contract.Prelude

import Contract.Address (PaymentPubKeyHash(..), StakePubKeyHash(..), getWalletAddress)
import Contract.Credential (Credential(..), StakingCredential(..))
import Contract.Monad (Contract, liftContractM, runContractInEnv)
import Contract.PlutusData (PlutusData)
import Contract.ScriptLookups as Lookups
import Contract.Test.Plutip (withKeyWallet)
import Contract.TxConstraints (TxConstraint(..), TxConstraints, singleton)
import Contract.Value (lovelaceValueOf)
import Data.BigInt as BigInt
import Effect.Exception (throw)
import HelloWorld.Api (initialize)
import HsmWallet (makeHsmWallet)
import Test.HelloWorld.EnvRunner (EnvRunner)
import Test.Spec (Spec, describe, it)
import Util (buildBalanceSignAndSubmitTx, maxWait, waitForTx, withOurLogger)

spec :: EnvRunner -> Spec Unit
spec envRunner = do
  describe "Signing tests" $ do
    traverse_ (_ $ envRunner)
      [ signingTest
      ]

useRunnerSimple :: forall a. Contract () a -> EnvRunner -> Aff Unit
useRunnerSimple contract runner = do
  runner \env alice ->
    runContractInEnv (withOurLogger "apiTest.log" env)
      $ withKeyWallet alice
      $ void contract

signingTest :: EnvRunner -> Spec Unit
signingTest = it "basic signing test" <$> useRunnerSimple do
  hsmWallet <- liftAff $ makeHsmWallet
  adr <- withKeyWallet hsmWallet getWalletAddress >>= liftContractM "no wallet"
  (key /\ skey) <- liftContractM "bad adr" =<< case unwrap adr of
    { addressCredential: PubKeyCredential key, addressStakingCredential: mskey } -> do
      skey <- case mskey of
        Nothing -> pure Nothing
        Just (StakingHash (PubKeyCredential skey)) -> pure $ Just skey
        _ -> liftEffect $ throw "bad staking credential"
      pure $ Just $ key /\ skey
    _ -> pure Nothing

  let
    lookups :: Lookups.ScriptLookups PlutusData
    lookups = mempty

    constraints :: TxConstraints Unit Unit
    constraints = singleton (MustPayToPubKeyAddress (PaymentPubKeyHash key) (StakePubKeyHash <$> skey) Nothing Nothing (lovelaceValueOf $ BigInt.fromInt 30_000_000))
  txid <- buildBalanceSignAndSubmitTx lookups constraints
  _ <- waitForTx maxWait adr txid
  withKeyWallet hsmWallet $ initialize 1 0
