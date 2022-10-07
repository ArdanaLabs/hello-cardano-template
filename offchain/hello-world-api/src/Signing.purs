module Signing
  ( getPubKey
  , hsmSignTx
  ) where

import Contract.Prelude

import Contract.Address (ByteArray)
import Contract.Prim.ByteArray (byteArrayToHex)
import Contract.Transaction (Ed25519Signature(..), PublicKey(..), Transaction(Transaction), TransactionWitnessSet, Vkey(..), _vkeys)
import Ctl.Internal.Serialization (toBytes)
import Ctl.Internal.Serialization as Serialization
import Data.Lens (set)
import Data.String (trim)
import Node.Buffer.Class (toString)
import Node.ChildProcess (defaultExecSyncOptions, execFileSync)
import Node.Encoding (Encoding(UTF8))
import Untagged.Union (asOneOf)

-- | This type is intended to represent a generic foreign signing function
type Signer = ByteArray -> Aff String

getPubKey :: Aff PublicKey
getPubKey = execAff "signer" [ "getPubKey" ] <#> trim >>> PublicKey

hsmSignTx :: PublicKey -> Transaction -> Aff TransactionWitnessSet
hsmSignTx = signTx cmdSigner

cmdSigner :: Signer
cmdSigner tx = do
  let args = [ "sign", byteArrayToHex tx ]
  execAff "signer" args <#> trim

execAff :: String -> Array String -> Aff String
execAff cmd args = liftEffect $ execFileSync cmd args defaultExecSyncOptions >>= toString UTF8

signTx :: Signer -> PublicKey -> Transaction -> Aff TransactionWitnessSet
signTx signer pubKey (Transaction tx) = do
  txBody <- liftEffect $ Serialization.convertTxBody tx.body
  hash <- liftEffect $ Serialization.hashTransaction txBody
  sig <- signer $ toBytes $ asOneOf hash
  let wit = Just [ wrap $ (Vkey pubKey) /\ (Ed25519Signature sig) ]
  pure $ set _vkeys wit mempty
