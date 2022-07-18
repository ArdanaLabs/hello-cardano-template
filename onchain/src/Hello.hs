{-# LANGUAGE UndecidableInstances #-}

module Hello (
  helloValidator,
  helloLogic,
  helloValidatorHash,
  helloAddress,
  helloWorldHexString,
  paramHelloCBOR,
  HelloRedemer (..),
) where

import GHC.Generics qualified as GHC
import Generics.SOP (Generic, I (I))
import Utils (closedTermToHexString, validatorToHexString)

import Plutus.V1.Ledger.Address (Address (..))
import Plutus.V1.Ledger.Credential (Credential (..))
import Plutus.V1.Ledger.Scripts (Validator, ValidatorHash)

import Plutarch.Prelude

import Plutarch.Api.V1 (PScriptContext, PValidator, mkValidator, validatorHash)
import Plutarch.Builtin (pforgetData)
import Plutarch.DataRepr (PIsDataReprInstances (PIsDataReprInstances))
import Plutarch.Extensions.Api (passert, pgetContinuingDatum)
import Plutarch.Unsafe (punsafeCoerce)

import Plutarch.Extra.TermCont (pmatchC)

data HelloRedemer (s :: S)
  = Inc (Term s (PDataRecord '[]))
  | Spend (Term s (PDataRecord '[]))
  deriving stock (GHC.Generic)
  deriving anyclass (Generic)
  deriving anyclass (PIsDataRepr)
  deriving
    (PlutusType, PIsData, PEq)
    via (PIsDataReprInstances HelloRedemer)

helloWorldHexString :: String
helloWorldHexString = validatorToHexString helloValidator

paramHelloCBOR :: String
paramHelloCBOR = closedTermToHexString paramValidator

helloValidator :: Validator
helloValidator = mkValidator (paramValidator #$ pforgetData (pdata (1 :: Term _ PInteger)))

helloValidatorHash :: ValidatorHash
helloValidatorHash = validatorHash helloValidator

helloAddress :: Address
helloAddress = Address (ScriptCredential helloValidatorHash) Nothing

paramValidator :: ClosedTerm (PData :--> PValidator)
paramValidator = plam $ \dCountBy dn dunit dsc -> do
  let n = pfromData (punsafeCoerce dn)
      u = pfromData (punsafeCoerce dunit)
      res cb = paramValidator' # cb # n # u # dsc
   in ptryFrom @(PAsData PInteger) dCountBy $ \(_, i) -> popaque $ res i

-- TODO Try wrapping the counter in a newtype to
-- test shareing newtypes/datatypes with apps

paramValidator' :: ClosedTerm (PInteger :--> PInteger :--> HelloRedemer :--> PScriptContext :--> PUnit)
paramValidator' = plam $ \countBy n r sc -> unTermCont $ do
  pmatchC r >>= \case
    Inc _ -> do
      datum <- pgetContinuingDatum @PInteger sc
      pure $ helloLogic # countBy # n # pfromData datum
    Spend _ -> pure $ pcon PUnit

helloLogic :: ClosedTerm (PInteger :--> PInteger :--> PInteger :--> PUnit)
helloLogic = plam $ \countBy n m -> unTermCont $ passert "int was not correct" $ n + countBy #== m
