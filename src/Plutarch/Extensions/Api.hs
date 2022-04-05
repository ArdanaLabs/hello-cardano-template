module Plutarch.Extensions.Api (getContinuingDatum,passert) where

import Plutarch.Prelude

import Plutarch.Api.V1 (
  PAddress,
  PDatum(..),
  PDatumHash,
  PScriptContext (..),
  PScriptPurpose (PSpending),
  PTuple,
  PTxInInfo (..),
  PTxInfo (..),
  PTxOut,
  PTxOutRef (..),
  PMaybeData(..),
 )
import Plutarch.Extensions.Monad (tlet, tletField, tmatch, tmatchField)
import Plutarch.Unsafe (punsafeCoerce)

getContinuingOutputs :: Term s (PScriptContext :--> PBuiltinList PTxOut)
getContinuingOutputs = phoistAcyclic $
  plam $ \sc -> unTermCont $ do
    txinfo <- tletField @"txInfo" sc
    outs <- tletField @"outputs" txinfo
    pure $
      pmatch (findOwnInput # sc) $ \case
        PJust te -> unTermCont $ do
          resolved <- tletField @"resolved" te
          outAdr <- tletField @"address" resolved
          pure $ pfilter # (matches # outAdr) #$ pmap # plam pfromData # outs
        PNothing -> ptraceError "can't get any continuing outputs"
  where
    matches :: Term s (PAddress :--> PTxOut :--> PBool)
    matches = phoistAcyclic $
      plam $ \adr txOut -> unTermCont $ do
        outAdr <- tletField @"address" txOut
        pure $ adr #== outAdr

findOwnInput :: Term s (PScriptContext :--> PMaybe PTxInInfo)
findOwnInput = phoistAcyclic $
  plam $ \sc -> unTermCont $ do
    PScriptContext te <- tmatch sc
    pure $
      pmatch (pfromData $ pfield @"purpose" # te) $ \case
        PSpending outRef' -> unTermCont $ do
          outRef <- tletField @"_0" outRef'
          PTxInfo txinfo <- tmatchField @"txInfo" te
          is <- tlet $ pmap # plam pfromData #$ pfromData $ pfield @"inputs" # txinfo
          pure $
            pfind # (matches # outRef) # is
        _ -> pcon PNothing
  where
    matches :: Term s (PTxOutRef :--> PTxInInfo :--> PBool)
    matches = phoistAcyclic $
      plam $ \outref txininfo -> unTermCont $ do
        PTxOutRef outref' <- tmatch outref
        outRefId <- tletField @"id" outref'
        PTxInInfo txininfo' <- tmatch txininfo
        PTxOutRef inOutRef <- tmatchField @"outRef" txininfo'
        inOutRefId <- tletField @"id" inOutRef
        pure $
          outRefId #== inOutRefId

findDatum :: Term s (PDatumHash :--> PTxInfo :--> PMaybe PDatum)
findDatum = phoistAcyclic $
  plam $ \dh txinfo -> unTermCont $ do
    txInfoData <- tletField @"data" txinfo
    maybeEnt <- tlet $ pfind # (matches # dh) # txInfoData
    pure $
      pmatch maybeEnt $ \case
        PNothing -> pcon PNothing
        PJust x -> pcon $ PJust $ pfromData $ pfield @"_1" # x
  where
    matches :: Term s (PDatumHash :--> PAsData (PTuple PDatumHash PDatum) :--> PBool)
    matches = phoistAcyclic $
      plam $ \dh dataTupe -> unTermCont $ do
        tupe <- tlet $ pfromData dataTupe
        pure $
          dh #== pfromData (pfield @"_0" # tupe)

getContinuingDatum :: PIsData p => Term s PScriptContext -> TermCont s (Term s p)
getContinuingDatum sc = do
  txinfo <- tletField @"txInfo" sc
  out <- unsingleton <$> tlet (getContinuingOutputs # sc)
  PDJust datumHash <- tmatchField @"datumHash" out
  PJust datum <- tmatch $ findDatum # (pfield @"_0" # datumHash) # txinfo
  PDatum dat <- tmatch datum
  pure $ pfromData (punsafeCoerce dat)



-- Probably belongs in list not api
-- should also be more polymorphic
unsingleton :: PLift a => Term s (PBuiltinList a) -> Term s a
unsingleton list = unTermCont $ do
  PCons x xs <- tmatch list
  PNil <- tmatch xs
  pure x

passert :: Term s PString -> Term s PBool -> TermCont s (Term s PUnit)
passert msg bool = pure $ pif bool (pcon PUnit) (ptraceError msg)
