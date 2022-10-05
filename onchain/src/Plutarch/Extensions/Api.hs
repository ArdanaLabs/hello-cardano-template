module Plutarch.Extensions.Api (
  pgetContinuingDatum,
  passert,
  passert_,
  pfindOwnInput,
  pgetContinuingOutput,
) where

import Plutarch.Prelude

import Control.Monad (void)
import Plutarch.Api.V2 (
  PAddress,
  PDatum (PDatum),
  POutputDatum (..),
  PScriptContext (..),
  PScriptPurpose (PSpending),
  PTxInInfo,
  PTxOut,
  PTxOutRef,
 )
import Plutarch.Extensions.Data (parseData)
import Plutarch.Extensions.List (unsingleton)
import Plutarch.Extensions.Monad (pmatchFieldC)
import Plutarch.Extra.TermCont (pletC, pmatchC)

{- | enfroces that there is a unique continuing output gets it's Datum
 - and converts it to the desired type via pfromData
-}
pgetContinuingDatum :: forall p s. (PTryFrom PData (PAsData p), PIsData p) => Term s PScriptContext -> TermCont s (Term s p)
pgetContinuingDatum ctx = do
  out <- pgetContinuingOutput ctx
  datum <-
    pmatchFieldC @"datum" out >>= \case
      PNoOutputDatum _ -> fail "no data"
      POutputDatum datum -> pure $ pfield @"outputDatum" # datum
      POutputDatumHash _ -> fail "datum hash not supported"
  PDatum d <- pmatchC datum
  parseData d

pgetContinuingOutput :: forall s. Term s PScriptContext -> TermCont s (Term s PTxOut)
pgetContinuingOutput ctx = do
  ctxF <- tcont $ pletFields @["txInfo", "purpose"] ctx
  txInfoF <- tcont $ pletFields @["inputs", "outputs", "datums"] $ getField @"txInfo" ctxF
  PSpending outRef <- pmatchC $ getField @"purpose" ctxF
  pure $
    unsingleton $
      pgetContinuingOutputs
        # getField @"inputs" txInfoF
        # getField @"outputs" txInfoF
        # (pfield @"_0" # outRef)

-- | fails with provided message if the bool is false otherwise returns unit
passert :: Term s PString -> Term s PBool -> TermCont s (Term s POpaque)
passert msg bool = pletC $ pif bool (popaque $ pcon PUnit) (ptraceError msg)

passert_ :: Term s PString -> Term s PBool -> TermCont s ()
passert_ msg bool = void $ passert msg bool

-- Taken from plutarch-extra to use current api version

pgetContinuingOutputs :: Term s (PBuiltinList PTxInInfo :--> PBuiltinList PTxOut :--> PTxOutRef :--> PBuiltinList PTxOut)
pgetContinuingOutputs = phoistAcyclic $
  plam $ \inputs outputs outRef ->
    pmatch (pfindOwnInput # inputs # outRef) $ \case
      PJust tx -> do
        let resolved = pfield @"resolved" # tx
            outAddr = pfield @"address" # resolved
        pfilter # (matches # outAddr) # outputs
      PNothing ->
        ptraceError "can't get any continuing outputs"
  where
    matches :: Term s (PAddress :--> PTxOut :--> PBool)
    matches = phoistAcyclic $
      plam $ \adr txOut ->
        adr #== pfield @"address" # txOut

pfindOwnInput :: Term s (PBuiltinList PTxInInfo :--> PTxOutRef :--> PMaybe PTxInInfo)
pfindOwnInput = phoistAcyclic $
  plam $ \inputs outRef ->
    pfind # (matches # outRef) # inputs
  where
    matches :: Term s (PTxOutRef :--> PTxInInfo :--> PBool)
    matches = phoistAcyclic $
      plam $ \outref txininfo ->
        outref #== pfield @"outRef" # txininfo
