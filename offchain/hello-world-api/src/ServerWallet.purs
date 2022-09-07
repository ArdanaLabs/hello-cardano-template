module ServerWallet (makeServerWallet) where

import Contract.Prelude

import Cardano.Types.Transaction (PublicKey(..), TransactionOutput(..), Utxos)
import Cardano.Types.TransactionUnspentOutput (TransactionUnspentOutput(TransactionUnspentOutput))
import Cardano.Types.Value (Value(..), mkCoin, unwrapNonAdaAsset)
import Contract.Address (PaymentPubKey(..), PaymentPubKeyHash(..), PubKeyHash(..), pubKeyHashAddress)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.Ord.Min (Min(..))
import Effect.Exception (throw)
import Serialization (publicKeyFromBech32, publicKeyHash)
import Serialization.Address (addressFromBech32, enterpriseAddress, enterpriseAddressToAddress, keyHashCredential)
import Signing (getServerPubKey, serverSignTx)
import Unsafe.Coerce (unsafeCoerce)
import Wallet.Key (KeyWallet(..))

makeServerWallet :: Aff KeyWallet
makeServerWallet = do
  pubKey@(PublicKey bech32) <- getServerPubKey
  pubKey2 <- case publicKeyFromBech32 bech32 of
    Nothing -> liftEffect $ throw "pub key conversion error"
    Just adr -> pure $ adr
  pure $ KeyWallet
    { address : \network -> pure $
          pubKey2 #
          publicKeyHash
          >>> keyHashCredential
          >>> { network, paymentCred: _ }
          >>> enterpriseAddress
          >>> enterpriseAddressToAddress
    , paymentKey : undefined -- TODO better error
    , selectCollateral : selectCollateral
    , signTx : serverSignTx pubKey
    , stakeKey : Nothing
    }


-- Taken from ctl
-- TODO this is not great
selectCollateral :: Utxos -> Maybe TransactionUnspentOutput
selectCollateral utxos = unwrap <<< unwrap <$> flip
  foldMapWithIndex
  utxos
  \input output ->
    let
      txuo = AdaOut $ TransactionUnspentOutput { input, output }
      Value ada naa = _value txuo
      onlyAda = all (all ((==) zero)) (unwrapNonAdaAsset naa)
      bigAda = ada >= mkCoin 5_000_000
    in
      if onlyAda && bigAda then Just $ Min txuo
      else Nothing

_value :: AdaOut -> Value
_value
  (AdaOut (TransactionUnspentOutput { output: TransactionOutput { amount } })) =
  amount

-- A wrapper around a UTxO, ordered by ada value
newtype AdaOut = AdaOut TransactionUnspentOutput

derive instance Newtype AdaOut _

instance Eq AdaOut where
  eq a b
    | Value a' _ <- _value a
    , Value b' _ <- _value b = eq a' b'

instance Ord AdaOut where
  compare a b
    | Value a' _ <- _value a
    , Value b' _ <- _value b = compare a' b'
