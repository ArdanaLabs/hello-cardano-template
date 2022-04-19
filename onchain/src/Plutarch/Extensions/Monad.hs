{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Plutarch.Extensions.Monad (tletField, tmatchField) where

import Plutarch.Prelude

import GHC.TypeLits (KnownNat)
import Plutarch.DataRepr (PDataFields (PFields))
import Plutarch.DataRepr.Internal (PLabelIndex, PUnLabel)
import Plutarch.DataRepr.Internal.HList (IndexList)
import Plutarch.Extra.TermCont

tletField ::
  forall name p s a as n.
  ( PDataFields p
  , as ~ PFields p
  , n ~ PLabelIndex name as
  , KnownNat n
  , a ~ PUnLabel (IndexList n as)
  , PIsData a
  ) =>
  Term s p ->
  TermCont s (Term s a)
tletField t = pletC $ pfromData $ pfield @name # t

tmatchField ::
  forall name p s a as n.
  ( PDataFields p
  , as ~ PFields p
  , n ~ PLabelIndex name as
  , KnownNat n
  , a ~ PUnLabel (IndexList n as)
  , PIsData a
  , PMatch a
  ) =>
  Term s p ->
  TermCont s (a s)
tmatchField t = pmatchC $ pfromData $ pfield @name # t
