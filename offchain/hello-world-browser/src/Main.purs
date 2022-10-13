module Main
  ( getConfigParams
  , main
  ) where

import Contract.Prelude

import Aeson (printJsonDecodeError, JsonDecodeError, decodeJsonString)
import Affjax (get, printError)
import Affjax.ResponseFormat (string)
import Affjax.StatusCode (StatusCode(StatusCode))
import Contract.Config (NetworkId(..), PrivatePaymentKey(..), PrivatePaymentKeySource(..), PrivateStakeKey(..), PrivateStakeKeySource(..), privateKeyFromBytes, testnetConfig, testnetNamiConfig)
import Contract.Monad (ConfigParams, ServerConfig)
import Contract.Wallet (WalletSpec(..))
import Ctl.Internal.Cardano.TextEnvelope (TextEnvelopeType(..), printTextEnvelopeDecodeError, textEnvelopeBytes)
import Data.Bifunctor (lmap)
import Effect (Effect)
import Effect.Class.Console (warn)
import Effect.Exception (error, throw)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import HelloWorld.AppM (runAppM)
import HelloWorld.Cookie (getCookie)
import HelloWorld.Page.Home as Home

type CtlRuntimeConfig =
  { ogmiosConfig :: ServerConfig
  , datumCacheConfig :: ServerConfig
  , ctlServerConfig :: ServerConfig
  }

getConfigParams :: Aff (ConfigParams ())
getConfigParams = do
  res <- get string $ "/dist/" <> ctlRuntimeConfigFileName
  case res of
    Left affjaxError -> do
      warn $ printError affjaxError
      ctlRuntimeConfigFileNotFoundWarning
      pure testnetConfig
    Right response -> do
      case response.status of
        StatusCode 200 -> do
          case (decodeJsonString response.body) :: Either JsonDecodeError CtlRuntimeConfig of
            Left decodeError -> do
              warn $ "Unable to decode the provided " <> ctlRuntimeConfigFileName <> ":\n"
                <> printJsonDecodeError decodeError
                <> "\n"
                <> "Falling back to the default configuration."
              warn $ show response
              pure testnetConfig
            Right ctlRuntimeConfig ->
              pure $ testnetConfig
                { ogmiosConfig = ctlRuntimeConfig.ogmiosConfig
                , datumCacheConfig = ctlRuntimeConfig.datumCacheConfig
                , ctlServerConfig = Just ctlRuntimeConfig.ctlServerConfig
                }
        StatusCode code -> do
          warn $ "HTTP status code: " <> show code
          ctlRuntimeConfigFileNotFoundWarning
          pure testnetConfig
  where
  ctlRuntimeConfigFileName = "ctl-runtime-config.json"
  ctlRuntimeConfigFileNotFoundWarning =
    warn $ "Unable to get the CTL runtime configuration file. \n"
      <> "Falling back to the default configuration.\n"
      <> "Configure the CTL runtime by adding a "
      <> ctlRuntimeConfigFileName
      <> " to the dist directory."

main :: Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody
    contractConfig <- useKeyWallet >>= case _ of
      true -> do
        configParams <- getConfigParams
        walletSpec <- loadWalletSpec
        networkId <- loadNetworkId
        -- the mainnetConfig is defined as `testnetConfig { networkId = MainnetId }` in CTL
        pure $ configParams
          { walletSpec = Just walletSpec
          , networkId = networkId
          }
      false -> pure $ testnetNamiConfig { logLevel = Warn }
    let
      store =
        { contractConfig
        , lastOutput: Nothing
        }
    rootComponent <- runAppM store Home.component
    runUI rootComponent unit body

useKeyWallet :: Aff Boolean
useKeyWallet =
  isJust <$> getCookie "paymentKey"

loadWalletSpec :: Aff WalletSpec
loadWalletSpec = do
  mPaymentKeyStr <- getCookie "paymentKey"
  paymentKeyStr <- liftM (error "payment key not found in the cookie") mPaymentKeyStr
  paymentKeyBytes <- liftEither $ lmap (error <<< printTextEnvelopeDecodeError) $
    textEnvelopeBytes paymentKeyStr PaymentSigningKeyShelleyed25519
  paymentKey <- liftM (error "Unable to decode private payment key")
    $ PrivatePaymentKey
    <$> privateKeyFromBytes (wrap paymentKeyBytes)

  getCookie "stakeKey" >>= case _ of
    Nothing -> pure $ UseKeys (PrivatePaymentKeyValue paymentKey) Nothing
    Just stakeKeyStr -> do
      stakeKeyBytes <- liftEither $ lmap (error <<< printTextEnvelopeDecodeError) $
        textEnvelopeBytes stakeKeyStr StakeSigningKeyShelleyed25519
      let stakeKey = PrivateStakeKey <$> privateKeyFromBytes (wrap stakeKeyBytes)
      pure $ UseKeys (PrivatePaymentKeyValue paymentKey) (PrivateStakeKeyValue <$> stakeKey)

loadNetworkId :: Aff NetworkId
loadNetworkId = do
  mNetworkId <- getCookie "networkId"
  networkId <- liftM (error "network ID not found in the cookie") mNetworkId
  case networkId of
    "MainnetId" -> pure MainnetId
    "TestnetId" -> pure TestnetId
    _ -> liftEffect $ throw "Unknown network ID"
