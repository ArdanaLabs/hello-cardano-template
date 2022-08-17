{-# LANGUAGE UndecidableInstances #-}

module Hello (
  helloValidator,
  helloLogic,
  helloValidatorHash,
  helloAddress,
  helloWorldHexString,
  paramHelloCBOR,
  trivialCBOR,
  HelloRedemer (..),
) where

import Data.Default (Default (def))
import Utils (closedTermToHexString, validatorToHexString)

import PlutusLedgerApi.V1.Address (Address (..))
import PlutusLedgerApi.V1.Credential (Credential (..))
import PlutusLedgerApi.V1.Scripts (Validator, ValidatorHash)

import Plutarch.Prelude

import Plutarch.Api.V1 (PScriptContext, PValidator, mkValidator, validatorHash)
import Plutarch.Builtin (pforgetData)
import Plutarch.Extensions.Api (passert, pgetContinuingDatum)
import Plutarch.Unsafe (punsafeCoerce)

import Plutarch.Extra.TermCont (pmatchC)

data HelloRedemer (s :: S)
  = Inc (Term s (PDataRecord '[]))
  | Spend (Term s (PDataRecord '[]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PEq)

instance DerivePlutusType HelloRedemer where type DPTStrat _ = PlutusTypeData

helloWorldHexString :: String
helloWorldHexString = validatorToHexString helloValidator

paramHelloCBOR :: Maybe String
paramHelloCBOR = closedTermToHexString paramValidator

trivialCBOR :: Maybe String
trivialCBOR = closedTermToHexString trivial

helloValidator :: Validator
helloValidator = mkValidator def (paramValidator #$ pforgetData (pdata (1 :: Term _ PInteger)))

helloValidatorHash :: ValidatorHash
helloValidatorHash = validatorHash helloValidator

helloAddress :: Address
helloAddress = Address (ScriptCredential helloValidatorHash) Nothing

trivial :: ClosedTerm (PData :--> PValidator)
trivial = plam $ \_ _ _ _ -> popaque $ pcon PUnit

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
