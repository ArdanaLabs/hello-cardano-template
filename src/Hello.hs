module Hello (helloScriptBytes, helloScript) where


import Plutus.V1.Ledger.Scripts (Script)

import Plutarch (ClosedTerm, compile)
import Plutarch.Prelude
import Plutarch.Unsafe ( punsafeCoerce )
import Plutarch.Api.V1 (PDatum(PDatum),PMaybeData (PDJust), PScriptContext)
import Plutarch.Extensions.Api (findDatum, getContinuingOutputs)
import Plutarch.Extensions.Monad (tlet, tletField, tmatch, tmatchField)

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
validator' = plam $ \n _unit sc -> unTermCont $ do
  txinfo <- tletField @"txInfo" sc
  out <- unsingleton <$> tlet (getContinuingOutputs # sc)
  PDJust datumHash <- tmatchField @"datumHash" out
  PJust datum <- tmatch $ findDatum # (pfield @"_0" # datumHash) # txinfo
  PDatum dat <- tmatch datum
  (datumAsInt :: Term s PInteger) <- tlet $ pfromData (punsafeCoerce dat)
  pure $
    pif
      ((n + 1) #== datumAsInt)
      (pcon PUnit)
      perror

unsingleton :: PLift a => Term s (PBuiltinList a) -> Term s a
unsingleton list = unTermCont $ do
  PCons x xs <- tmatch list
  PNil <- tmatch xs
  pure x

