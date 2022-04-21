module Plutarch.Extensions.List (
  unsingleton,
) where

import Plutarch.Extensions.Monad (tmatch)
import Plutarch.Prelude

-- | Enforces that a list has only one element and returns it
unsingleton :: PLift a => Term s (PBuiltinList a) -> Term s a
unsingleton list = unTermCont $ do
  PCons x xs <- tmatch list
  PNil <- tmatch xs
  pure x
