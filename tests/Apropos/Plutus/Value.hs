module Apropos.Plutus.Value (
  valueGenSelfTests,
  MultiValueProp,
  MultiValue,
  ) where

import Apropos

import Apropos.Plutus.AssetClass
import Apropos.Plutus.Integer
import Apropos.Plutus.SingletonValue

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (fromGroup)
--import Plutus.V1.Ledger.Value (AssetClass)

type MultiValue = [SingletonValue]

data MultiValueProp
  = HasSomeAda
  deriving stock (Eq,Ord,Show,Enum,Bounded)

instance Enumerable MultiValueProp where
  enumerated = [minBound..maxBound]

instance LogicalModel MultiValueProp where
  logic = Yes

propsFor :: MultiValueProp -> [SingletonValueProp]
propsFor HasSomeAda = [AC IsAda,Amt IsPositive]

instance HasLogicalModel MultiValueProp MultiValue where
  satisfiesProperty p = any $ satisfiesAll (propsFor p)

instance HasPermutationGenerator MultiValueProp MultiValue where
  generators =
    [ Morphism
        { name = "Add " ++ show p
        , match = Yes
        , contract = add p
        , morphism = \xs -> do
            x <- genSatisfying $ All (Var <$> propsFor p)
            return $ x:xs
        }
    | p <- enumerated ]
    ++
    [ Morphism
        { name = "Remove " ++ show p
        , match = Var p
        , contract = remove p
        , morphism = return . filter (not . satisfiesAll (propsFor p))
        }
    | p <- enumerated ]

instance HasParameterisedGenerator MultiValueProp MultiValue where
  parameterisedGenerator = buildGen $ pure []
  -- TODO better base gen

valueGenSelfTests :: TestTree
valueGenSelfTests =
  testGroup "valueGenSelfTests" $
    fromGroup
      <$> permutationGeneratorSelfTest
        True
        (const @Bool @(Morphism MultiValueProp MultiValue) True)
        (pure [])

