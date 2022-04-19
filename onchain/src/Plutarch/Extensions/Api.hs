module Plutarch.Extensions.Api (
  getContinuingDatum,
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
import Plutarch.TryFrom (PTryFrom)

{- | enfroces that there is a unique continuing output gets it's Datum
 - and converts it to the desired type via pfromData
-}
getContinuingDatum :: forall p s. (PTryFrom PData (PAsData p)) => Term s PScriptContext -> TermCont s (Term s (PAsData p))
getContinuingDatum sc = do
  ctxF <- tcont $ pletFields @["txInfo", "purpose"] sc
  pmatchC (getField @"purpose" ctxF) >>= \case
    PSpending outRef -> do
      txInfoF <- tcont $ pletFields @["inputs", "outputs", "datums"] $ getField @"txInfo" ctxF
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
    _ ->
      pure perror

-- | fails with provided message if the bool is false otherwise returns unit
passert :: Term s PString -> Term s PBool -> Term s PUnit
passert msg bool = pif bool (pcon PUnit) (ptraceError msg)
