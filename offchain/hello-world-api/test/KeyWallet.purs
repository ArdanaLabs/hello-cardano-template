module KeyWallet
  ( spec
  ) where

import Contract.Prelude
import UnitTest (helloUnitTest)
import Contract.Monad
  ( runContract_
  , configWithLogLevel
  )
import Contract.Wallet.KeyFile(mkKeyWalletFromFiles)
import Data.Log.Level (LogLevel(Trace))
import Node.Process(lookupEnv)
import Serialization.Address (NetworkId(TestnetId))
import Test.Spec(Spec,describe,it)
import Test.Spec.Assertions(shouldReturn)


spec :: Spec Unit
spec = do
  describe "Full tests" do
    it "integration test" $ shouldReturn integrationTest unit

integrationTest :: Aff Unit
integrationTest = do
  testResourcesDir <- liftEffect $ fromMaybe "." <$> lookupEnv "TEST_RESOURCES"
  wallet <- mkKeyWalletFromFiles
              (testResourcesDir <> "/wallet.skey") 
              (pure $ testResourcesDir <> "/staking.skey")
  cfg <- configWithLogLevel TestnetId wallet Trace
  runContract_ cfg $ do
    helloUnitTest
    -- TODO this seems to work but also hangs
