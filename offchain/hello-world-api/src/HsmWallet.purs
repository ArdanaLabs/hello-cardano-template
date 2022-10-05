module HsmWallet (makeHsmWallet) where

import Contract.Prelude

import Ctl.Internal.BalanceTx.Collateral.Select (selectCollateral) as Collateral
import Ctl.Internal.Cardano.Types.Transaction (PublicKey(..), UtxoMap)
import Ctl.Internal.Cardano.Types.TransactionUnspentOutput (TransactionUnspentOutput)
import Contract.Address (NetworkId)
import Contract.Config (PrivatePaymentKey(..))
import Data.Array (fromFoldable)
import Effect.Exception (throw)
import Ctl.Internal.QueryM.Ogmios (CoinsPerUtxoUnit)
import Ctl.Internal.Serialization (publicKeyFromBech32, publicKeyHash)
import Ctl.Internal.Serialization.Address (Address, enterpriseAddress, enterpriseAddressToAddress, keyHashCredential)
import Signing (getCmd, getPubKey, hsmSignTx)
import Unsafe.Coerce (unsafeCoerce)
import Ctl.Internal.Wallet.Key (KeyWallet(..))

-- | Returns a KeyWallet which
-- internally interfaces with the
-- signing cli
-- since the private key can't be read
-- the paymentKey method has to throw an error
makeHsmWallet :: String -> Aff KeyWallet
makeHsmWallet varName = do
  cmd <- getCmd varName
  pubKey@(PublicKey bech32) <- getPubKey cmd
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
    , signTx: hsmSignTx cmd pubKey
    , paymentKey: PrivatePaymentKey $ unsafeCoerce "tried to use the private key of a yubikey"
    , stakeKey: Nothing
    }

