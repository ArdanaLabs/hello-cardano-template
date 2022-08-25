module Main
  ( main
  ) where

import Contract.Prelude

import Cardano.TextEnvelope (TextEnvelopeType(..), textEnvelopeBytes)
import Contract.Config (NetworkId(..), PrivatePaymentKey(..), PrivatePaymentKeySource(..), PrivateStakeKey(..), PrivateStakeKeySource(..), privateKeyFromBytes, testnetConfig, testnetNamiConfig)
import Effect (Effect)
import Effect.Exception (error, throw)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import HelloWorld.AppM (runAppM)
import HelloWorld.Page.Home as Home
import KeyWallet.Cookie (getCookie)
import Wallet.Spec (WalletSpec(UseKeys))

main :: Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody
    contractConfig <- useKeyWallet >>= case _ of
      true -> do
        walletSpec <- loadWalletSpec
        networkId <- loadNetworkId
        pure $ testnetConfig { walletSpec = Just walletSpec, networkId = networkId }
      false -> do
        pure $ testnetNamiConfig { logLevel = Warn }
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
  paymentKeyBytes <- textEnvelopeBytes paymentKeyStr PaymentSigningKeyShelleyed25519
  paymentKey <- liftM (error "Unable to decode private payment key")
    $ PrivatePaymentKey
    <$> privateKeyFromBytes (wrap paymentKeyBytes)

  getCookie "stakeKey" >>= case _ of
    Nothing -> pure $ UseKeys (PrivatePaymentKeyValue paymentKey) Nothing
    Just stakeKeyStr -> do
      stakeKeyBytes <- textEnvelopeBytes stakeKeyStr StakeSigningKeyShelleyed25519
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
