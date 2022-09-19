module Test.HelloWorld.Signing
  (spec
  ) where

import Contract.Prelude

import Contract.Address (PaymentPubKeyHash(..), StakePubKeyHash(..), addressToBech32, getWalletAddress)
import Contract.Config (PrivatePaymentKey(..))
import Contract.Credential (Credential(..), StakingCredential(..))
import Contract.Log (logError')
import Contract.Monad (Contract, liftContractAffM, liftContractM, runContractInEnv)
import Contract.ScriptLookups as Lookups
import Contract.Test.Plutip (withKeyWallet)
import Contract.TxConstraints (TxConstraint(..), TxConstraints)
import Contract.TxConstraints as Constraints
import Contract.Value (lovelaceValueOf)
import Data.BigInt as BigInt
import Data.Time.Duration (Seconds(..), fromDuration)
import Debug (traceM)
import Effect.Aff (delay)
import Effect.Exception (throw)
import HelloWorld.Api (enoughForFees, initialize)
import Plutus.Types.Address (Address(..))
import Serialization (publicKeyFromPrivateKey, publicKeyHash)
import ServerWallet (makeServerWallet)
import Signing (getServerPubKey)
import Test.HelloWorld.EnvRunner (EnvRunner, plutipConfig)
import Test.Spec (Spec, describe, it)
import Types.PlutusData (PlutusData(Constr, Integer))
import Types.TxConstraints (singleton)
import Util (buildBalanceSignAndSubmitTx, maxWait, waitForTx, withOurLogger)
import Wallet.Key (keyWalletPrivatePaymentKey)

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
  serverWallet <- liftAff $ makeServerWallet
  adr <- withKeyWallet serverWallet getWalletAddress >>= liftContractM "no wallet"
  (key /\ skey) <- liftContractM "bad adr" =<< case adr of
    Address{ addressCredential : PubKeyCredential key , addressStakingCredential : mskey } -> do
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
    constraints =  singleton (MustPayToPubKeyAddress (PaymentPubKeyHash key) (StakePubKeyHash <$> skey) Nothing Nothing (lovelaceValueOf $ BigInt.fromInt 30_000_000))
  txid <- buildBalanceSignAndSubmitTx lookups constraints
  _ <- waitForTx maxWait adr txid
  withKeyWallet serverWallet $ initialize 1 0
