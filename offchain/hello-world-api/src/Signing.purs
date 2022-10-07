module Signing
  ( getPubKey
  , hsmSignTx
  , getCmd
  ) where

import Contract.Prelude

import Contract.Address (ByteArray)
import Contract.Prim.ByteArray (byteArrayToHex)
import Contract.Transaction (Ed25519Signature(..), PublicKey(..), Transaction(Transaction), TransactionWitnessSet, Vkey(..), _vkeys)
import Ctl.Internal.Serialization (toBytes)
import Ctl.Internal.Serialization as Serialization
import Data.Lens (set)
import Data.String (trim)
import Effect.Aff (makeAff)
import Effect.Exception (throw)
import Node.Buffer.Class (toString)
import Node.ChildProcess (ExecResult, defaultExecOptions, execFile)
import Node.Encoding (Encoding(UTF8))
import Node.Process (lookupEnv)
import Untagged.Union (asOneOf)

type Signer = ByteArray -> Aff String

getPubKey :: Aff PublicKey
getPubKey = do
  (res :: ExecResult) <- execAff "signer" [ "getPubKey" ]
  PublicKey <<< trim <$> (liftEffect $ toString UTF8 res.stdout)

hsmSignTx :: PublicKey -> Transaction -> Aff TransactionWitnessSet
hsmSignTx = signTx cmdSigner

cmdSigner :: Signer
cmdSigner a = do
  let args = [ "sign", byteArrayToHex a ]
  (res :: ExecResult) <- execAff "signer" args
  liftEffect $ trim <$> toString UTF8 res.stdout

execAff :: String -> Array String -> Aff ExecResult
execAff file args = makeAff $ \callBack -> do
  _child <- execFile file args defaultExecOptions (callBack <<< Right)
  pure mempty

signTx :: Signer -> PublicKey -> Transaction -> Aff TransactionWitnessSet
signTx signer pubKey (Transaction tx) = do
  txBody <- liftEffect $ Serialization.convertTxBody tx.body
  hash <- liftEffect $ Serialization.hashTransaction txBody
  sig <- signer $ toBytes $ asOneOf hash
  let wit = Just [ wrap $ (Vkey pubKey) /\ (Ed25519Signature sig) ]
  pure $ set _vkeys wit mempty
