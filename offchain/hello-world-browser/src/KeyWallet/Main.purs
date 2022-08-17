module KeyWallet.Main
  ( main
  ) where

import Contract.Prelude

import Contract.Config (NetworkId(..), mainnetConfig, testnetConfig)
import Data.BigInt as BigInt
import Data.UInt as UInt
import Effect (Effect)
import Effect.Exception (error)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import HelloWorld.AppM (runAppM)
import HelloWorld.Page.Home as Home
import Plutip.Server (withPlutipContractEnv)
import Plutip.Types (PlutipConfig)
import Wallet.Key (KeyWallet, keyWalletPrivatePaymentKey, keyWalletPrivateStakeKey)
import Wallet.Spec (WalletSpec(UseKeys), PrivatePaymentKeySource(PrivatePaymentKeyValue), PrivateStakeKeySource(PrivateStakeKeyValue))

main :: Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody

    withPlutipContractEnv config [ BigInt.fromInt 20_000_000 ] \env wallet -> do
      walletSpec <- loadKeyWallet wallet
      let
        contractConfig =
          if (unwrap env).config.networkId == MainnetId then
            mainnetConfig { walletSpec = Just walletSpec }
          else
            testnetConfig { walletSpec = Just walletSpec }
      let
        store =
          { contractConfig
          , lastOutput: Nothing
          }
      rootComponent <- runAppM store Home.component
      runUI rootComponent unit body

config :: PlutipConfig
config =
  { host: "127.0.0.1"
  , port: UInt.fromInt 8082
  , logLevel: Error
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

loadKeyWallet :: KeyWallet -> Aff WalletSpec
loadKeyWallet wallet = do
  let paymentKey = keyWalletPrivatePaymentKey wallet
  stakeKey <- liftM (error "Unable to decode private stake key") $ keyWalletPrivateStakeKey wallet
  pure $ UseKeys (PrivatePaymentKeyValue paymentKey) (Just $ PrivateStakeKeyValue stakeKey)
