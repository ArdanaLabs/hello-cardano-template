module ServerWallet (makeServerWallet) where

import Contract.Prelude

import BalanceTx.Collateral.Select (selectCollateral) as Collateral
import Cardano.Types.Transaction (PublicKey(..), TransactionOutput(..), UtxoMap)
import Cardano.Types.TransactionUnspentOutput (TransactionUnspentOutput(TransactionUnspentOutput))
import Cardano.Types.Value (Value(..), mkCoin, unwrapNonAdaAsset)
import Contract.Address (NetworkId, PaymentPubKey(..), PaymentPubKeyHash(..), PubKeyHash(..), pubKeyHashAddress)
import Contract.Config (PrivatePaymentKey(..))
import Contract.Log (logError')
import Contract.Transaction (Transaction(..), TransactionWitnessSet(..))
import Control.Monad.Error.Class (throwError)
import Data.Array (fromFoldable)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.Ord.Min (Min(..))
import Debug (traceM)
import Deserialization.WitnessSet as Deserialization.WitnessSet
import Effect.Exception (throw)
import QueryM.Ogmios (CoinsPerUtxoUnit)
import Serialization (publicKeyFromBech32, publicKeyFromPrivateKey, publicKeyHash)
import Serialization as Serialization
import Serialization.Address (Address, addressFromBech32, enterpriseAddress, enterpriseAddressToAddress, keyHashCredential)
import Signing (getServerPubKey, serverSignTx)
import Unsafe.Coerce (unsafeCoerce)
import Wallet.Key (KeyWallet(..))

makeServerWallet :: Aff KeyWallet
makeServerWallet = do
  pubKey@(PublicKey bech32) <- getServerPubKey
  pubKey2 <- case publicKeyFromBech32 bech32 of
    Nothing -> do
      log $ "got bad string: " <> bech32
      liftEffect $ throw "pub key conversion error"
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
    , signTx: serverSignTx pubKey
    , paymentKey: PrivatePaymentKey $ unsafeCoerce ""
    , stakeKey: Nothing
    }

