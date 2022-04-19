module Plutarch.Extensions.List (
  unsingleton,
) where

import Plutarch.Extra.TermCont (pmatchC)
import Plutarch.Prelude

-- | Enforces that a list has only one element and returns it
unsingleton :: PLift a => Term s (PBuiltinList a) -> Term s a
unsingleton list = unTermCont $ do
  PCons x xs <- pmatchC list
  PNil <- pmatchC xs
  pure x
