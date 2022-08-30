module Signing(signTx) where

import Contract.Prelude

import Cardano.Types.Transaction (Ed25519Signature(Ed25519Signature), PublicKey, Vkey(Vkey), _vkeys)
import Contract.Prim.ByteArray (byteArrayToHex)
import Contract.Transaction (Transaction(Transaction))
import Data.Lens (set)
import Serialization as Serialization
import Types.ByteArray (ByteArray)
import Unsafe.Coerce (unsafeCoerce)

type Signer = ByteArray -> Effect ByteArray
-- If it turns out this needs to be aff just lower the
-- liftEffect in signTx

signTx :: PublicKey -> Signer -> Transaction -> Aff Transaction
signTx pubKey signer (Transaction tx) = liftEffect do
  txBody <- Serialization.convertTxBody tx.body
  hash <- Serialization.hashTransaction txBody
  sig <-  signer $ unsafeCoerce hash
  let wit = Just [ wrap $ (Vkey pubKey) /\ (Ed25519Signature $ byteArrayToHex sig)  ]
  let witnessSet' = set _vkeys wit mempty
  pure $ Transaction $ tx { witnessSet = witnessSet' <> tx.witnessSet }
