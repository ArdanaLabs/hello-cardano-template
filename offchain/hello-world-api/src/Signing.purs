module Signing
  ( getServerPubKey
  , serverSignTx
  ) where

import Contract.Prelude

import Affjax (Error, Response, get, post, printError)
import Affjax.RequestBody (RequestBody(..))
import Affjax.ResponseFormat (string)
import Cardano.Types.Transaction (Ed25519Signature(Ed25519Signature), PublicKey(..), Vkey(Vkey), _vkeys)
import Contract.Prim.ByteArray (byteArrayToHex)
import Contract.Transaction (Transaction(Transaction), TransactionWitnessSet)
import Data.Lens (set)
import Effect.Exception (throw)
import Serialization (toBytes)
import Serialization as Serialization
import Types.ByteArray (ByteArray)
import Untagged.Union (asOneOf)

type Signer = ByteArray -> Aff String

getServerPubKey :: Aff PublicKey
getServerPubKey = PublicKey <$> (handleAffjax =<< get string "http://localhost:3000/pubkey")

-- FIXME I'm pretty sure this is the problem
-- it expects a bech32 string but is getting a hex string
-- imo it makes the most sense to fix this on the haskell side

serverSignTx :: PublicKey -> Transaction -> Aff TransactionWitnessSet
serverSignTx = signTx serverSigner

serverSigner :: ByteArray -> Aff String
serverSigner a = do
  handleAffjax =<< post string "http://localhost:3000/sign" (Just $ String $ byteArrayToHex a)

handleAffjax :: Either Error (Response String) -> Aff String
handleAffjax = case _ of
  Right resp -> pure resp.body
  Left err -> liftEffect $ throw $ printError err

signTx :: Signer -> PublicKey -> Transaction -> Aff TransactionWitnessSet
signTx signer pubKey (Transaction tx) = do
  txBody <- liftEffect $ Serialization.convertTxBody tx.body
  hash <- liftEffect $ Serialization.hashTransaction txBody
  sig <- signer $ toBytes $ asOneOf hash
  let wit = Just [ wrap $ (Vkey pubKey) /\ (Ed25519Signature sig) ]
  pure $ set _vkeys wit mempty
