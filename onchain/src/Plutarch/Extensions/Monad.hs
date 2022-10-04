{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Plutarch.Extensions.Monad (
  pletFieldC,
  pmatchFieldC,
  ptraceShowC,
) where

import Plutarch.Prelude

import GHC.TypeLits (KnownNat)
import Plutarch.DataRepr (PDataFields (PFields))
import Plutarch.DataRepr.Internal (PLabelIndex, PUnLabel)
import Plutarch.DataRepr.Internal.HList (IndexList)
import Plutarch.Extra.TermCont (pletC, pmatchC, ptraceC)

ptraceShowC :: PShow a => Term s a -> TermCont s ()
ptraceShowC x = ptraceC $ pshow x

pletFieldC ::
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
pletFieldC t = pletC $ pfield @name # t

pmatchFieldC ::
  forall name p s a as n.
  ( PDataFields p
  , as ~ PFields p
  , n ~ PLabelIndex name as
  , KnownNat n
  , a ~ PUnLabel (IndexList n as)
  , PIsData a
  , PlutusType a
  ) =>
  Term s p ->
  TermCont s (a s)
pmatchFieldC t = pmatchC $ pfield @name # t
