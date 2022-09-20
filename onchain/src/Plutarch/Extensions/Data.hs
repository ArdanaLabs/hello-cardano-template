module Plutarch.Extensions.Data (
  parseData,
  unsafeParseData,
) where

import Plutarch.Prelude

import Plutarch.Extra.TermCont (pletC)
import Plutarch.Unsafe (punsafeCoerce)

-- TODO is there a helper for this I couldn't find it
parseData :: forall a s. (PTryFrom PData (PAsData a), PIsData a) => Term s PData -> TermCont s (Term s a)
parseData d = pfromData . fst <$> tcont (ptryFrom @(PAsData a) d)

unsafeParseData :: forall a s. (PIsData a) => Term s PData -> TermCont s (Term s a)
unsafeParseData d = pletC $ pfromData $ punsafeCoerce d
