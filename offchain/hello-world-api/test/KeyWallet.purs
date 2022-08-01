module KeyWallet
  ( spec
  ) where

import Api(grabFreeAda)
import Util(ourLogger)
import Contract.Prelude
import Contract.Monad
  ( Contract
  , runContract
  )
import Contract.Config(testnetConfig)
import Node.Process(lookupEnv)
import Data.Log.Level (LogLevel(..))
import Test.Spec(Spec,describe,it)
import Test.Spec.Assertions(shouldReturn)
import Wallet.Spec
  (WalletSpec(UseKeys)
  ,PrivatePaymentKeySource(PrivatePaymentKeyFile)
  ,PrivateStakeKeySource(PrivateStakeKeyFile)
  )
import IntegrationTest
  (integrationTest
  ,lockTest
  ,lookupTest
  ,incTest
  ,postIncLookupTest
  ,unlockTest
  ,isSpentTest
  )

spec :: Spec Unit
spec = do
  describe "Testnet" do
    -- I'm not sure why but this only works if it goes first
    it "grabs any free ada from previous tests" $
      testContract $ grabFreeAda `shouldReturn` unit
    it "locks value" $
      testContract $ (lockTest 3 4) `shouldReturn` unit
    it "looks up datum" $
      testContract $ (lookupTest 3 4) `shouldReturn` 3
    it "increments" $
      testContract $ (incTest 3 4) `shouldReturn` unit
    it "looks up datum after inc" $
      testContract $ (postIncLookupTest 3 4) `shouldReturn` 7
    it "unlocks funds" $
      testContract $ (unlockTest 3 4) `shouldReturn` unit
    it "full integration test" $
      testContract $ (integrationTest 3 4) `shouldReturn` unit
    it "is spent works" $
      testContract $ isSpentTest 3 4

testContract :: Contract () Unit -> Aff Unit
testContract contract = do
  testResourcesDir <- liftEffect $ fromMaybe "./fixtures/" <$> lookupEnv "TEST_RESOURCES"
  let walletSpec = UseKeys (PrivatePaymentKeyFile $ testResourcesDir <> "/wallet.skey") (Just $ PrivateStakeKeyFile $ testResourcesDir <> "/staking.skey")
  let config = testnetConfig
        {walletSpec=Just walletSpec
        ,logLevel=Warn
        ,customLogger=Just $ ourLogger "./apiTest.log"
        }
  void <<< runContract config $ contract
