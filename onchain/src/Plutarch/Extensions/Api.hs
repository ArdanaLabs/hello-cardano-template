module Plutarch.Extensions.Api (
  pgetContinuingDatum,
  passert,
) where

import Plutarch.Prelude

import Plutarch.Api.V1 (
  PMaybeData (..),
  PScriptContext (..),
  PScriptPurpose (PSpending),
 )
import Plutarch.Extensions.List (unsingleton)
import Plutarch.Extensions.Monad (pmatchFieldC)
import Plutarch.Extra.Api (pgetContinuingOutputs, pparseDatum)
import Plutarch.Extra.TermCont (pmatchC)

{- | enfroces that there is a unique continuing output gets it's Datum
 - and converts it to the desired type via pfromData
-}
pgetContinuingDatum :: forall p s. (PTryFrom PData (PAsData p)) => Term s PScriptContext -> TermCont s (Term s (PAsData p))
pgetContinuingDatum ctx = do
  ctxF <- tcont $ pletFields @["txInfo", "purpose"] ctx
  txInfoF <- tcont $ pletFields @["inputs", "outputs", "datums"] $ getField @"txInfo" ctxF
  PSpending outRef <- pmatchC $ getField @"purpose" ctxF
  let out =
        unsingleton $
          pgetContinuingOutputs
            # getField @"inputs" txInfoF
            # getField @"outputs" txInfoF
            # (pfield @"_0" # outRef)
  PDJust datumHash <- pmatchFieldC @"datumHash" out
  PJust datum <-
    pmatchC $
      pparseDatum @p
        # (pfield @"_0" # datumHash)
        # getField @"datums" txInfoF
  pure datum

-- | fails with provided message if the bool is false otherwise returns unit
passert :: Term s PString -> Term s PBool -> TermCont s (Term s POpaque)
passert msg bool = pure $ pif bool (popaque $ pcon PUnit) (ptraceError msg)
