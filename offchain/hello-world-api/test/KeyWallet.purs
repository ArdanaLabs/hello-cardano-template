module KeyWallet
  ( spec
  ) where

import Contract.Prelude
import Api(cleanup)
import Contract.Monad
  ( Contract
  , runContract_
  , configWithLogLevel
  )
import Contract.Wallet.KeyFile(mkKeyWalletFromFiles)
import Data.Log.Level (LogLevel(Error))
import Serialization.Address (NetworkId(TestnetId))
import Test.Spec(Spec,describe,it)
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
  describe "Full tests" do
    it "locks value" $
      testContract $ lockTest `shouldReturn` unit
    it "looks up datum" $
      testContract $ lookupTest `shouldReturn` 3
    it "increments" $
      testContract $ incTest `shouldReturn` unit
    it "looks up datum after inc" $
      testContract $ postIncLookupTest `shouldReturn` 7
    it "unlocks" $
      testContract $ unlockTest `shouldReturn` unit
    it "full integrationTest" $
      testContract $ integrationTest `shouldReturn` unit
    it "cleans up" $
      testContract $ cleanup `shouldReturn` unit

testContract :: Contract () Unit -> Aff Unit
testContract contract = do
  wallet <- mkKeyWalletFromFiles
              (toFixturePath "wallet.skey")
              (Just $ toFixturePath "staking.skey")
  cfg <- configWithLogLevel TestnetId wallet Error
  runContract_ cfg contract
