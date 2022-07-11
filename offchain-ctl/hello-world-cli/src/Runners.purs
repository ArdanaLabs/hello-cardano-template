module Runners
  (runCmd
  -- todo these are exported to supress warnings
  ,readConfig
  ,makeConfig
  ) where

import Contract.Prelude

import Effect(Effect)
import Types(Command(..),Conf(..),ParsedOptions(..))
import Contract.Monad
  ( DefaultContractConfig
  , launchAff_
  , runContract_
  , configWithLogLevel
  )
import Data.Log.Level(LogLevel(Trace))
import Contract.Wallet.KeyFile(mkKeyWalletFromFiles)
import Node.FS.Sync (readTextFile)
import Node.Encoding (Encoding(UTF8))

readConfig :: ParsedOptions -> Effect Command
readConfig (ParsedOptions o)= do
  _txt <- readTextFile UTF8 o.configFile
  -- TODO parse config return real command
  pure undefined

runCmd :: Command -> Effect Unit
runCmd (Command {conf}) = launchAff_ do
  cfg <- makeConfig conf
  -- TODO I should probably case over sub command types here
  runContract_ cfg $ do
    pure unit

makeConfig :: Conf -> Aff DefaultContractConfig
makeConfig
  (Conf{walletPath,stakingPath,network}) = do
  wallet <- mkKeyWalletFromFiles walletPath $ Just stakingPath
  configWithLogLevel network wallet Trace


