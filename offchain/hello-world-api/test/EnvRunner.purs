module Test.HelloWorld.EnvRunner
  ( getEnvRunner
  , EnvRunner
  , Mode(..)
  , plutipConfig
  ) where

import Prelude

import Contract.Config (testnetConfig)
import Contract.Monad (ContractEnv, withContractEnv)
import Contract.Test.Plutip (PlutipConfig, withPlutipContractEnv)
import Data.BigInt as BigInt
import Data.Log.Level (LogLevel(Warn))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.UInt as UInt
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Node.Process (lookupEnv)
import Wallet.Key (KeyWallet, privateKeysToKeyWallet)
import Wallet.KeyFile (privatePaymentKeyFromFile, privateStakeKeyFromFile)

data Mode = Local | Testnet
type EnvRunner = (ContractEnv () -> KeyWallet -> Aff Unit) -> Aff Unit

getEnvRunner :: Mode -> Aff EnvRunner
getEnvRunner Local = pure $ withPlutipContractEnv plutipConfig [ BigInt.fromInt 20_000_000 ]
getEnvRunner Testnet = do
  testResourcesDir <- liftEffect $ fromMaybe "./fixtures/" <$> lookupEnv "TEST_RESOURCES"
  key <- privatePaymentKeyFromFile $ testResourcesDir <> "/wallet.skey"
  stakeKey <- privateStakeKeyFromFile $ testResourcesDir <> "/staking.skey"
  let keyWallet = privateKeysToKeyWallet key (Just stakeKey)
  pure
    $ \f -> withContractEnv (testnetConfig { logLevel = Warn }) $ \env -> f (env :: ContractEnv ()) (keyWallet :: KeyWallet)

plutipConfig :: PlutipConfig
plutipConfig =
  { host: "127.0.0.1"
  , port: UInt.fromInt 8082
  , logLevel: Warn
  -- Server configs are used to deploy the corresponding services.
  , ogmiosConfig:
      { port: UInt.fromInt 1338
      , host: "127.0.0.1"
      , secure: false
      , path: Nothing
      }
  , ogmiosDatumCacheConfig:
      { port: UInt.fromInt 10000
      , host: "127.0.0.1"
      , secure: false
      , path: Nothing
      }
  , ctlServerConfig:
      { port: UInt.fromInt 8083
      , host: "127.0.0.1"
      , secure: false
      , path: Nothing
      }
  , postgresConfig:
      { host: "127.0.0.1"
      , port: UInt.fromInt 5433
      , user: "ctxlib"
      , password: "ctxlib"
      , dbname: "ctxlib"
      }
  }
