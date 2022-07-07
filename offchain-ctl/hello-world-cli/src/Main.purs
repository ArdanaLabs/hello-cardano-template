module Main
  ( main
  , runUnitTest
  ) where

import Contract.Prelude
import UnitTest (helloUnitTest)
import Contract.Monad
  ( launchAff_
  , runContract_
  , configWithLogLevel
  )
import Contract.Wallet.KeyFile(mkKeyWalletFromFiles)
import Data.Log.Level (LogLevel(Trace))
import Effect.Class (liftEffect)
import Serialization.Address (NetworkId(TestnetId))
import Parse(getCmd)

main :: Effect Unit
main = launchAff_ $ loop
  where
    -- TODO this has a bug where your keystrokes start to count twice
    loop = do
      cmd <- getCmd
      log $ show cmd
      loop

runUnitTest :: Effect Unit
runUnitTest = launchAff_ $ do
  wallet <- mkKeyWalletFromFiles "wallet.skey" $ Just "staking.skey"
  cfg <- configWithLogLevel TestnetId wallet Trace
  runContract_ cfg $ do
    helloUnitTest
