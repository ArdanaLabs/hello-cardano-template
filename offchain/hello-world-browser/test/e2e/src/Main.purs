module HelloWorld.Test.E2E.Main where

import Contract.Prelude

import Data.BigInt as BigInt
import Data.Maybe (Maybe(..), isJust)
import Data.UInt as UInt
import Effect.Aff (bracket, launchAff_)
import Effect.Class (liftEffect)
import Effect.Exception (throw, error)
import HelloWorld.Test.E2E.Env as Env
import HelloWorld.Test.E2E.Helpers (closeStaticServer, startStaticServer)
import HelloWorld.Test.E2E.TestPlans.KeyWallet as Key
import HelloWorld.Test.E2E.TestPlans.NamiWallet as Nami
import Node.Process (lookupEnv)
import Plutip.Server (withPlutipContractEnv)
import Plutip.Types (PlutipConfig)
import Wallet.Key (keyWalletPrivatePaymentKey, keyWalletPrivateStakeKey)
import Wallet.KeyFile (formatPaymentKey, formatStakeKey)

data TestWallet
  = Nami
  | Key

getTestWallet :: Effect TestWallet
getTestWallet =
  lookupEnv Env.testWallet >>= case _ of
    Just "KeyWallet" -> pure Key
    Just "NamiWallet" -> pure Nami
    _ -> throw "Unknow test wallet"

main ∷ Effect Unit
main = do
  noRuntime <- isJust <$> lookupEnv Env.noRuntime
  if noRuntime then
    log "skip the test since there's no ctl-runtime"
  else do
    helloWorldBrowserIndex <- lookupEnv Env.helloWorldBrowserIndex

    case helloWorldBrowserIndex of
      Nothing -> throw "HELLO_WORLD_BROWSER_INDEX not set"
      Just index -> launchAff_ do
        withPlutipContractEnv config [ BigInt.fromInt 2_000_000_000 ] \env wallet -> do
          paymentKey <- liftM (error "Failed to format payment key") $ formatPaymentKey (keyWalletPrivatePaymentKey wallet)
          let stakeKey = keyWalletPrivateStakeKey wallet >>= formatStakeKey
          bracket (startStaticServer paymentKey stakeKey (unwrap env).config.networkId index) closeStaticServer $ \_ -> do
            liftEffect getTestWallet >>= case _ of
              Key -> Key.runTestPlans
              Nami -> Nami.runTestPlans

config :: PlutipConfig
config =
  { host: "127.0.0.1"
  , port: UInt.fromInt 8082
  , logLevel: Error
  -- Server configs are used to deploy the corresponding services.
  , ogmiosConfig:
      { port: UInt.fromInt 1337
      , host: "127.0.0.1"
      , secure: false
      , path: Nothing
      }
  , ogmiosDatumCacheConfig:
      { port: UInt.fromInt 9999
      , host: "127.0.0.1"
      , secure: false
      , path: Nothing
      }
  , ctlServerConfig:
      { port: UInt.fromInt 8081
      , host: "127.0.0.1"
      , secure: false
      , path: Nothing
      }
  , postgresConfig:
      { host: "127.0.0.1"
      , port: UInt.fromInt 5432
      , user: "ctxlib"
      , password: "ctxlib"
      , dbname: "ctxlib"
      }
  }