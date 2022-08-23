module Plutarch.Extensions.Data (
  parseData,
  ptryFromData,
  unsafeParseData,
) where

import Plutarch.Prelude

import Plutarch.Extra.TermCont (pletC)
import Plutarch.Unsafe (punsafeCoerce)

-- TODO is there a helper for this I couldn't find it
parseData :: forall a s. (PTryFrom PData (PAsData a), PIsData a) => Term s PData -> TermCont s (Term s a)
parseData d = pfromData <$> ptryFromData' d

ptryFromData :: forall a s. PTryFrom PData (PAsData a) => Term s PData -> Term s (PAsData a)
ptryFromData = unTermCont <$> ptryFromData'

ptryFromData' :: forall a s. PTryFrom PData (PAsData a) => Term s PData -> TermCont s (Term s (PAsData a))
ptryFromData' x = fst <$> tcont (ptryFrom @(PAsData a) x)

-- SHOULD NOT BE USED IN PRODUCTION VALIDATORS
-- usefull for testing
unsafeParseData :: forall a s. (PIsData a) => Term s PData -> TermCont s (Term s a)
unsafeParseData d = pletC $ pfromData $ punsafeCoerce d
