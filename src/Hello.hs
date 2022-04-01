module Hello (helloScriptBytes, helloScript) where

import Plutarch (ClosedTerm, compile)
import Plutarch.Prelude

import Plutus.V1.Ledger.Scripts (Script)

import Plutarch.Api.V1 (PMaybeData (PDJust), PScriptContext)
import Plutarch.Builtin (pforgetData)

import Codec.Serialise (serialise)
import Data.ByteString.Lazy (ByteString)
import Plutarch.Local.Api (findDatum, getContinuingOutputs)
import Plutarch.Local.Monad (tlet, tletField, tmatch, tmatchField)

helloScriptBytes :: ByteString
helloScriptBytes = serialise helloScript

helloScript :: Script
helloScript = compile validator

validator :: ClosedTerm (PInteger :--> PUnit :--> PScriptContext :--> PUnit)
validator = plam $ \n _unit sc -> unTermCont $ do
  txinfo <- tletField @"txInfo" sc
  outs <- tlet $ getContinuingOutputs # sc
  PCons out mustBeEmpty <- tmatch outs
  PNil <- tmatch mustBeEmpty
  PDJust datumHash' <- tmatchField @"datumHash" out
  datumHash <- tletField @"_0" datumHash'
  PJust datum <- tmatch $ findDatum # datumHash # txinfo
  datumAsData <- tlet $ pforgetData $ pdata datum
  pure $
    pif
      (pforgetData (pdata (n + 1)) #== datumAsData)
      (pcon PUnit)
      perror
