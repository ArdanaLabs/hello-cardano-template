module Hello (helloScriptBytes, helloScript) where

import Plutus.V1.Ledger.Scripts (Script)

import Plutarch (ClosedTerm, compile)
import Plutarch.Api.V1 (PScriptContext)
import Plutarch.Extensions.Api (getContinuingDatum, passert)
import Plutarch.Prelude

import Codec.Serialise (serialise)
import Data.ByteString.Lazy (ByteString)

helloScriptBytes :: ByteString
helloScriptBytes = serialise helloScript

helloScript :: Script
helloScript = compile validator

validator :: ClosedTerm (PAsData PInteger :--> PAsData PUnit :--> PAsData PScriptContext :--> PUnit)
validator = plam $ \dn dunit dsc -> validator' # pfromData dn # pfromData dunit # pfromData dsc

-- TODO Try wrapping the counter in a newtype to
-- test shareing newtypes/datatypes with apps

validator' :: ClosedTerm (PInteger :--> PUnit :--> PScriptContext :--> PUnit)
validator' = plam $ \n _unit sc ->
  unTermCont $
    getContinuingDatum sc >>= passert "int was not correct" . ((n + 1) #==)
