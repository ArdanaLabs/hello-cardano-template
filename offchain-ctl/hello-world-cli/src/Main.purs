module Main
  ( main
  ) where

import Contract.Prelude
import UnitTest (helloUnitTest)
import Contract.Monad
  ( launchAff_
  , runContract_
  , configWithLogLevel
  )
import Contract.Wallet.KeyFile(mkKeyWalletFromFile)
import Serialization.Address (NetworkId(TestnetId))
import Data.Log.Level (LogLevel(Trace))

main :: Effect Unit
main = launchAff_ $ do
  wallet' <- mkKeyWalletFromFile "wallet.skey"
  case wallet' of
    Just wallet -> do
      cfg <- configWithLogLevel TestnetId wallet Trace
      runContract_ cfg helloUnitTest
    Nothing -> undefined
