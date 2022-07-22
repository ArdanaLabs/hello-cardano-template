module KeyWallet
  ( spec
  ) where

import Contract.Prelude
import Api(helloScript,sendDatumToScript,setDatumAtScript,redeemFromScript,datumLookup)
import Contract.Monad
  ( runContract_
  , configWithLogLevel
  , liftContractAffM
  )
import Contract.Wallet.KeyFile(mkKeyWalletFromFiles)
import Contract.Scripts (validatorHash)
import Data.Log.Level (LogLevel(Error))
import Serialization.Address (NetworkId(TestnetId))
import Test.Spec(Spec,describe,it)
import Test.Spec.Assertions(shouldReturn,shouldEqual)
import IntegrationTest(integrationTest)


toFixturePath :: String -> String
toFixturePath = (<>) "./fixtures/"

spec :: Spec Unit
spec = do
  describe "Full tests" do
    it "integration test" $ shouldReturn runIntegrationTest unit

runIntegrationTest :: Aff Unit
runIntegrationTest = do
  wallet <- mkKeyWalletFromFiles
              (toFixturePath "wallet.skey")
              (Just $ toFixturePath "staking.skey")
  cfg <- configWithLogLevel TestnetId wallet Error
  runContract_ cfg integrationTest
