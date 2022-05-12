module Hello (helloValidator, helloLogic, helloValidatorHash, helloAddress) where

import Plutus.V1.Ledger.Address (Address (..))
import Plutus.V1.Ledger.Credential (Credential (..))
import Plutus.V1.Ledger.Scripts (Validator, ValidatorHash)

import Plutarch.Api.V1 (PScriptContext, mkValidator, validatorHash)
import Plutarch.Extensions.Api (passert, pgetContinuingDatum)
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

helloValidator :: Validator
helloValidator = mkValidator validator

helloValidatorHash :: ValidatorHash
helloValidatorHash = validatorHash helloValidator

helloAddress :: Address
helloAddress = Address (ScriptCredential helloValidatorHash) Nothing

validator :: ClosedTerm (PData :--> PData :--> PScriptContext :--> POpaque)
validator = plam $ \dn dunit dsc -> do
  let n = pfromData (punsafeCoerce dn)
      u = pfromData (punsafeCoerce dunit)
      res = validator' # n # u # dsc
   in popaque res

-- TODO Try wrapping the counter in a newtype to
-- test shareing newtypes/datatypes with apps

validator' :: ClosedTerm (PInteger :--> PUnit :--> PScriptContext :--> PUnit)
validator' = plam $ \n _unit sc -> unTermCont $ do
  datum <- pgetContinuingDatum @PInteger sc
  pure $ helloLogic # n # pfromData datum

helloLogic :: ClosedTerm (PInteger :--> PInteger :--> PUnit)
helloLogic = plam $ \n m -> unTermCont $ passert "int was not correct" $ n + 1 #== m
