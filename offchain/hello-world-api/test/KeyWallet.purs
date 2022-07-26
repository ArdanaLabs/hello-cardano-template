module KeyWallet
  ( spec
  ) where

import Api(grabFreeAda)
import Contract.Prelude
import Contract.Monad
  ( Contract
  , runContract_
  , configWithLogLevel
  )
import Contract.Wallet.KeyFile(mkKeyWalletFromFiles)
import Node.Process(lookupEnv)
import Data.Log.Level (LogLevel(Error))
import Serialization.Address (NetworkId(TestnetId))
import Test.Spec(Spec,describe,it,itOnly)
import Test.Spec.Assertions(shouldReturn)
import IntegrationTest
  (integrationTest
  ,lockTest
  ,lookupTest
  ,incTest
  ,postIncLookupTest
  ,unlockTest
  )


toFixturePath :: String -> String
toFixturePath = (<>) "./fixtures/"

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

testContract :: Contract () Unit -> Aff Unit
testContract contract = do
  testResourcesDir <- liftEffect $ fromMaybe "." <$> lookupEnv "TEST_RESOURCES"
  wallet <- mkKeyWalletFromFiles
              (testResourcesDir <> "/wallet.skey")
              (pure $ testResourcesDir <> "/staking.skey")
  cfg <- configWithLogLevel TestnetId wallet Error
  runContract_ cfg contract
