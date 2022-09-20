module ServerWallet (makeServerWallet) where

import Contract.Prelude

import BalanceTx.Collateral.Select (selectCollateral) as Collateral
import Cardano.Types.Transaction (PublicKey(..), UtxoMap)
import Cardano.Types.TransactionUnspentOutput (TransactionUnspentOutput)
import Contract.Address (NetworkId)
import Contract.Config (PrivatePaymentKey(..))
import Data.Array (fromFoldable)
import Effect.Exception (throw)
import QueryM.Ogmios (CoinsPerUtxoUnit)
import Serialization (publicKeyFromBech32, publicKeyHash)
import Serialization.Address (Address, enterpriseAddress, enterpriseAddressToAddress, keyHashCredential)
import Signing (getServerCmd, getServerPubKey, serverSignTx)
import Unsafe.Coerce (unsafeCoerce)
import Wallet.Key (KeyWallet(..))

makeServerWallet :: Aff KeyWallet
makeServerWallet = do
  serverCmd <- getServerCmd
  pubKey@(PublicKey bech32) <- getServerPubKey serverCmd
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
    , signTx: serverSignTx serverCmd pubKey
    , paymentKey: PrivatePaymentKey $ unsafeCoerce "tried to use a the private key of a yubikey"
    , stakeKey: Nothing
    }

