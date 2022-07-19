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
import Serialization.Address (NetworkId(TestnetId))
import Test.Spec(Spec,describe,it)
import Test.Spec.Assertions(shouldReturn)


toFixturePath :: String -> String
toFixturePath = (<>) "./fixtures/" 

spec :: Spec Unit
spec = do
  describe "Full tests" do
    it "integration test" $ shouldReturn integrationTest unit

integrationTest :: Aff Unit
integrationTest = do
  wallet <- mkKeyWalletFromFiles (toFixturePath "wallet.skey") $ Just $ toFixturePath "staking.skey"
  cfg <- configWithLogLevel TestnetId wallet Trace
  runContract_ cfg $ do
    helloUnitTest
    -- TODO this seems to work but also hangs
