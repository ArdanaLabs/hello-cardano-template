module HsmWallet (makeHsmWallet) where

import Contract.Prelude

import Contract.Address (NetworkId)
import Contract.Config (PrivatePaymentKey(..))
import Contract.Wallet.Key (KeyWallet(..))
import Ctl.Internal.BalanceTx.Collateral.Select as Collateral
import Ctl.Internal.Cardano.Types.Transaction (PublicKey(..), UtxoMap)
import Ctl.Internal.Cardano.Types.TransactionUnspentOutput (TransactionUnspentOutput)
import Ctl.Internal.QueryM.Ogmios (CoinsPerUtxoUnit)
import Ctl.Internal.Serialization (publicKeyFromBech32, publicKeyHash)
import Ctl.Internal.Serialization.Address (Address, enterpriseAddress, enterpriseAddressToAddress, keyHashCredential)
import Data.Array (fromFoldable)
import Effect.Exception (throw)
import Signing (getPubKey, hsmSignTx)
import Unsafe.Coerce (unsafeCoerce)

-- | Returns a KeyWallet which
-- internally interfaces with the
-- signing cli
-- since the private key can't be read
-- the paymentKey method has to throw an error
makeHsmWallet :: Aff KeyWallet
makeHsmWallet = do
  pubKey@(PublicKey bech32) <- getPubKey
  pubKey2 <- case publicKeyFromBech32 bech32 of
    Nothing -> do
      liftEffect $ throw $ "pub key conversion error on string: " <> bech32
    Just adr -> pure $ adr
  let
    address :: NetworkId -> Aff Address
    address network = pure $ pubKey2 # publicKeyHash
      >>> keyHashCredential
      >>> { network, paymentCred: _ }
      >>> enterpriseAddress
      >>> enterpriseAddressToAddress

    selectCollateral
      :: CoinsPerUtxoUnit
      -> Int
      -> UtxoMap
      -> Effect (Maybe (Array TransactionUnspentOutput))
    selectCollateral coinsPerUtxoByte maxCollateralInputs utxos = map fromFoldable
      <$> Collateral.selectCollateral coinsPerUtxoByte maxCollateralInputs utxos

  pure $ KeyWallet
    { address
    , selectCollateral
    , signTx: hsmSignTx pubKey
    , paymentKey: PrivatePaymentKey $ unsafeCoerce "tried to use the private key of a yubikey"
    , stakeKey: Nothing
    }

