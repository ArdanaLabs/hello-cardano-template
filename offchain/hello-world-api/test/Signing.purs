module Test.HelloWorld.Signing
  (spec
  ) where

import Contract.Prelude

import Contract.Address (PaymentPubKeyHash(..), StakePubKeyHash(..), getWalletAddress)
import Contract.Credential (Credential(..), StakingCredential(..))
import Contract.Log (logError')
import Contract.Monad (Contract, liftContractAffM, liftContractM, runContractInEnv)
import Contract.ScriptLookups as Lookups
import Contract.Test.Plutip (withKeyWallet)
import Contract.TxConstraints (TxConstraint(..), TxConstraints)
import Contract.TxConstraints as Constraints
import Effect.Exception (throw)
import HelloWorld.Api (enoughForFees, initialize)
import Plutus.Types.Address (Address(..))
import Serialization (publicKeyHash)
import ServerWallet (makeServerWallet)
import Signing (getServerPubKey)
import Test.HelloWorld.EnvRunner (EnvRunner, plutipConfig)
import Test.Spec (Spec, describe, it)
import Types.PlutusData (PlutusData(Constr, Integer))
import Types.TxConstraints (singleton)
import Util (buildBalanceSignAndSubmitTx)

spec :: EnvRunner -> Spec Unit
spec envRunner = do
  describe "HelloWorld.Api" $ do
    traverse_ (_ $ envRunner)
      [ signingTest
      ]

useRunnerSimple :: forall a. Contract () a -> EnvRunner -> Aff Unit
useRunnerSimple contract runner = do
  runner \env alice -> do
    runContractInEnv env
      $ withKeyWallet alice
      $ void contract

signingTest :: EnvRunner -> Spec Unit
signingTest = it "basic signing test" <$> useRunnerSimple do
  logError' "0"
  serverWallet <- liftAff $ makeServerWallet
  logError' "1"
  adr <- withKeyWallet serverWallet getWalletAddress >>= liftContractM "no wallet"
  logError' "2"
  (key /\ skey) <- liftContractM "bad adr" =<< case adr of
    Address{ addressCredential : PubKeyCredential key , addressStakingCredential : mskey } -> do
      skey <- case mskey of
                    Nothing -> pure Nothing
                    Just (StakingHash (PubKeyCredential skey)) -> pure $ Just skey
                    _ -> liftEffect $ throw "bad staking credential"
      pure $ Just $ key /\ skey
    _ -> pure Nothing

  logError' "3"
  let
    lookups :: Lookups.ScriptLookups PlutusData
    lookups = mempty
    constraints :: TxConstraints Unit Unit
    constraints =  singleton (MustPayToPubKeyAddress (PaymentPubKeyHash key) (StakePubKeyHash <$> skey) Nothing enoughForFees)
  _ <- buildBalanceSignAndSubmitTx lookups constraints
  logError' "4"
  _ <- withKeyWallet serverWallet $ initialize 1 0
  logError' "5"
  pure unit
