module Test.KeyWallet (loadKeyWallet) where

import Contract.Prelude
import Cardano.TextEnvelope (TextEnvelopeType(PaymentSigningKeyShelleyed25519, StakeSigningKeyShelleyed25519), textEnvelopeBytes)
import Effect.Exception (error)
import Serialization (privateKeyFromBytes)
import Wallet.Spec
  ( WalletSpec(UseKeys)
  , PrivatePaymentKeySource(PrivatePaymentKeyValue)
  , PrivateStakeKeySource(PrivateStakeKeyValue)
  )
import Wallet.Key (PrivatePaymentKey(PrivatePaymentKey), PrivateStakeKey(PrivateStakeKey))

_paymentKeyStr :: String
_paymentKeyStr = "{\"type\":\"PaymentSigningKeyShelley_ed25519\",\"description\":\"PaymentSigningKey\",\"cborHex\":\"5820d071b5fc9e6f8d1cbc0a4b22dd2ce4fb8f537d0bfe3e6073a758e65c1591275e\"}"

_stakeKeyStr :: String
_stakeKeyStr = "{\"type\":\"StakeSigningKeyShelley_ed25519\",\"description\":\"StakeSigningKey\",\"cborHex\":\"58204b43bbce308317030a526ae92f9579ecafebaa0da20fa46ddc48e12e07de6472\"}"

loadKeyWallet :: Aff WalletSpec
loadKeyWallet = do
  paymentKeyBytes <- textEnvelopeBytes _paymentKeyStr PaymentSigningKeyShelleyed25519
  stakeKeyBytes <- textEnvelopeBytes _stakeKeyStr StakeSigningKeyShelleyed25519
  paymentKey <- liftM (error "Unable to decode private payment key")
    $ PrivatePaymentKey
    <$> privateKeyFromBytes (wrap paymentKeyBytes)
  stakeKey <- liftM (error "Unable to decode private stake key")
    $ PrivateStakeKey
    <$> privateKeyFromBytes (wrap stakeKeyBytes)
  pure $ UseKeys (PrivatePaymentKeyValue paymentKey) (Just $ PrivateStakeKeyValue stakeKey)
