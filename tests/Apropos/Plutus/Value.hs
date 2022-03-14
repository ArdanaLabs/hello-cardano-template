module Apropos.Plutus.Value (
  --valueGenSelfTests,
  --MultiValueProp,
  --MultiValue,
  ) where

{-
import Apropos

import Apropos.Plutus.SingletonValue (SingletonValueProp,SingletonValue)
import Apropos.Plutus.List (ListModel)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (fromGroup)

type MultiValue = [SingletonValue]
type MultiValueProp = ListModel SingletonValue

_valueGenSelfTests :: TestTree
_valueGenSelfTests =
  testGroup "valueGenSelfTests" $
    fromGroup
      <$> permutationGeneratorSelfTest
        True
        (const @Bool @(Morphism (ListModel SingletonValueProp) [SingletonValue]) True)
        (pure [])

-}
