module Plutarch.Extensions.Api (
  getContinuingDatum,
  findOwnInput,
  findDatum,
  passert,
) where

import Plutarch.Prelude

import Plutarch.Api.V1 (
  PAddress,
  PDatum (..),
  PDatumHash,
  PMaybeData (..),
  PScriptContext (..),
  PScriptPurpose (PSpending),
  PTuple,
  PTxInInfo (..),
  PTxInfo (..),
  PTxOut,
  PTxOutRef (..),
 )
import Plutarch.Extensions.List (unsingleton)
import Plutarch.Extensions.Monad (pletFieldC, pmatchFieldC)
import Plutarch.Extra.TermCont (pletC, pmatchC)
import Plutarch.Unsafe (punsafeCoerce)

{- | gets a list of continuing outputs by finding
 - its own input and  returning a list of outputs with the same outAddress
-}
getContinuingOutputs :: Term s (PScriptContext :--> PBuiltinList PTxOut)
getContinuingOutputs = phoistAcyclic $
  plam $ \sc -> unTermCont $ do
    txinfo <- pletFieldC @"txInfo" sc
    outs <- pletFieldC @"outputs" txinfo
    pure $
      pmatch (findOwnInput # sc) $ \case
        PJust te -> unTermCont $ do
          resolved <- pletFieldC @"resolved" te
          outAdr <- pletFieldC @"address" resolved
          pure $ pfilter # (matches # outAdr) #$ pmap # plam pfromData # outs
        PNothing -> ptraceError "can't get any continuing outputs"
  where
    matches :: Term s (PAddress :--> PTxOut :--> PBool)
    matches = phoistAcyclic $
      plam $ \adr txOut -> unTermCont $ do
        outAdr <- pletFieldC @"address" txOut
        pure $ adr #== outAdr

{- | tries to finds the transaction's input
 - by looking for a txininfo in the inputs coresponding to the TxOutRef which the script purpose is spending
-}
findOwnInput :: Term s (PScriptContext :--> PMaybe PTxInInfo)
findOwnInput = phoistAcyclic $
  plam $ \sc -> unTermCont $ do
    PScriptContext te <- pmatchC sc
    pure $
      pmatch (pfromData $ pfield @"purpose" # te) $ \case
        PSpending outRef' -> unTermCont $ do
          outRef <- pletFieldC @"_0" outRef'
          PTxInfo txinfo <- pmatchFieldC @"txInfo" te
          is <- pletC $ pmap # plam pfromData #$ pfromData $ pfield @"inputs" # txinfo
          pure $
            pfind # (matches # outRef) # is
        _ -> pcon PNothing
  where
    matches :: Term s (PTxOutRef :--> PTxInInfo :--> PBool)
    matches = phoistAcyclic $
      plam $ \outref txininfo -> unTermCont $ do
        PTxOutRef outref' <- pmatchC outref
        outRefId <- pletFieldC @"id" outref'
        PTxInInfo txininfo' <- pmatchC txininfo
        PTxOutRef inOutRef <- pmatchFieldC @"outRef" txininfo'
        inOutRefId <- pletFieldC @"id" inOutRef
        pure $
          outRefId #== inOutRefId

-- | Looks up a datum by it's hash from the PTxInfo
findDatum :: Term s (PDatumHash :--> PTxInfo :--> PMaybe PDatum)
findDatum = phoistAcyclic $
  plam $ \dh txinfo -> unTermCont $ do
    txInfoData <- pletFieldC @"datums" txinfo
    maybeEnt <- pletC $ pfind # (matches # dh) # txInfoData
    pure $
      pmatch maybeEnt $ \case
        PNothing -> pcon PNothing
        PJust x -> pcon $ PJust $ pfromData $ pfield @"_1" # x
  where
    matches :: Term s (PDatumHash :--> PAsData (PTuple PDatumHash PDatum) :--> PBool)
    matches = phoistAcyclic $
      plam $ \dh dataTupe -> unTermCont $ do
        tupe <- pletC $ pfromData dataTupe
        pure $
          dh #== pfromData (pfield @"_0" # tupe)

{- | enfroces that there is a unique continuing output gets it's Datum
 - and converts it to the desired type via pfromData
-}
getContinuingDatum :: forall p s. PIsData p => Term s PScriptContext -> TermCont s (Term s p)
getContinuingDatum sc = do
  txinfo <- pletFieldC @"txInfo" sc
  out <- unsingleton <$> pletC (getContinuingOutputs # sc)
  PDJust datumHash <- pmatchFieldC @"datumHash" out
  PJust datum <- pmatchC $ findDatum # (pfield @"_0" # datumHash) # txinfo
  PDatum dat <- pmatchC datum
  pure $ pfromData (punsafeCoerce dat)

-- | fails with provided message if the bool is false otherwise returns unit
passert :: Term s PString -> Term s PBool -> Term s PUnit
passert msg bool = pif bool (pcon PUnit) (ptraceError msg)
