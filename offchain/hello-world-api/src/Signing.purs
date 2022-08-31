module Signing
  ( requestFromURIAff
  , signTx
  , fetch
  )
  where

import Contract.Prelude

import Cardano.Types.Transaction (Ed25519Signature(Ed25519Signature), PublicKey, Vkey(Vkey), _vkeys)
import Contract.Prim.ByteArray (byteArrayToHex)
import Contract.Transaction (Transaction(Transaction))
import Data.Lens (set)
import Effect.Aff as Aff
import Node.Encoding (Encoding(..))
import Node.Encoding as Enc
import Node.HTTP (Request, listen, createServer, setHeader, requestHeaders, requestMethod, requestURL, responseAsStream, requestAsStream, setStatusCode, onUpgrade)
import Node.HTTP.Client as Client
import Node.Stream as Stream
import Serialization as Serialization
import Types.ByteArray (ByteArray)
import Unsafe.Coerce (unsafeCoerce)
import Data.Maybe

type Signer = ByteArray -> Effect ByteArray
-- If it turns out this needs to be aff just lower the
-- liftEffect in signTx


-- simpleReq :: String -> Effect Unit
-- simpleReq uri = do
--   log ("GET " <> uri <> ":")
--   req <- Client.requestFromURI uri logResponse
--   end (Client.requestAsStream req) (const $ pure unit)

-- logResponse :: Client.Response -> Effect Unit
-- logResponse response = void do
--   log "Headers:"
--   logShow $ Client.responseHeaders response
--   log "Cookies:"
--   logShow $ Client.responseCookies response
--   log "Response:"
--   let responseStream = Client.responseAsStream response
--   pipe responseStream stdout

fetch :: String -> Aff String
fetch url = do
  resp <- requestFromURIAff url
  let responseStream = Client.responseAsStream resp
  respString <- liftEffect $ Stream.readString responseStream Nothing Enc.UTF8
  pure $ show respString

requestFromURIAff :: String -> Aff Client.Response
requestFromURIAff url =
  Aff.makeAff $ \cb -> do
    _req <- Client.requestFromURI
                url
                (Right >>> cb)
    pure mempty 


-- getPubkey :: Effect String
-- getPubkey = do
--   req <- Client.requestFromURI "http://localhost:3000/pubkey" (\resp -> pure unit)
--   -- decode JSON
--   -- extract value
--   return val


signTx :: PublicKey -> Signer -> Transaction -> Aff Transaction
signTx pubKey signer (Transaction tx) = liftEffect do
  txBody <- Serialization.convertTxBody tx.body
  hash <- Serialization.hashTransaction txBody
  sig <-  signer $ unsafeCoerce hash
  let wit = Just [ wrap $ (Vkey pubKey) /\ (Ed25519Signature $ byteArrayToHex sig)  ]
  let witnessSet' = set _vkeys wit mempty
  pure $ Transaction $ tx { witnessSet = witnessSet' <> tx.witnessSet }
