module Signing
  ( getServerPubKey
  , serverSignTx
  , getServerCmd
  ) where

import Contract.Prelude

import Cardano.Types.Transaction (Ed25519Signature(Ed25519Signature), PublicKey(..), Vkey(Vkey), _vkeys)
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
import Serialization (toBytes)
import Serialization as Serialization
import Types.ByteArray (ByteArray)
import Untagged.Union (asOneOf)

type Signer = ByteArray -> Aff String

getServerCmd :: String -> Aff String
getServerCmd varName = liftEffect $ do
  lookupEnv varName >>= case _ of
    Just cmd -> pure cmd
    Nothing -> throw $ "expected " <> varName <> " to be set"

getServerPubKey :: String -> Aff PublicKey
getServerPubKey serverCmd = do
  (res :: ExecResult) <- execAff serverCmd [ "getpubkey" ]
  PublicKey <<< trim <$> (liftEffect $ toString UTF8 res.stdout)

serverSignTx :: String -> PublicKey -> Transaction -> Aff TransactionWitnessSet
serverSignTx serverCmd = signTx (serverSigner serverCmd)

serverSigner :: String -> ByteArray -> Aff String
serverSigner serverCmd a = do
  let args = [ "sign", byteArrayToHex a ]
  (res :: ExecResult) <- execAff serverCmd args
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
