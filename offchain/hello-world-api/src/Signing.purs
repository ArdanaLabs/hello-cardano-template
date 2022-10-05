module Signing
  ( getPubKey
  , hsmSignTx
  , getCmd
  ) where

import Contract.Prelude

import Ctl.Internal.Cardano.Types.Transaction (Ed25519Signature(Ed25519Signature), PublicKey(..), Vkey(Vkey), _vkeys)
import Contract.Prim.ByteArray (byteArrayToHex)
import Contract.Transaction (Transaction(Transaction), TransactionWitnessSet)
import Data.Lens (set)
import Data.String (trim)
import Effect.Aff (makeAff)
import Effect.Exception (throw)
import Node.Buffer.Class (toString)
import Node.ChildProcess (ExecResult, defaultExecOptions, execFile)
import Node.Encoding (Encoding(UTF8))
import Node.Process (lookupEnv)
import Ctl.Internal.Serialization (toBytes)
import Ctl.Internal.Serialization as Serialization
import Ctl.Internal.Types.ByteArray (ByteArray)
import Untagged.Union (asOneOf)

type Signer = ByteArray -> Aff String

getCmd :: String -> Aff String
getCmd varName = liftEffect $ do
  lookupEnv varName >>= case _ of
    Just cmd -> pure cmd
    Nothing -> throw $ "expected " <> varName <> " to be set"

getPubKey :: String -> Aff PublicKey
getPubKey cmd = do
  (res :: ExecResult) <- execAff cmd [ "getPubKey" ]
  PublicKey <<< trim <$> (liftEffect $ toString UTF8 res.stdout)

hsmSignTx :: String -> PublicKey -> Transaction -> Aff TransactionWitnessSet
hsmSignTx cmd = signTx (cmdSigner cmd)

cmdSigner :: String -> Signer
cmdSigner cmd a = do
  let args = [ "sign", byteArrayToHex a ]
  (res :: ExecResult) <- execAff cmd args
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
