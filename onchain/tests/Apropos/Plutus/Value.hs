module Apropos.Plutus.Value (
  spec,
  MultiValueProp,
  MultiValue,
) where

import Apropos

import Apropos.Plutus.AssetClass (
  AssetClassProp (IsAda, IsDana, IsOther),
 )
import Apropos.Plutus.Integer (
  IntegerProp (IsPositive),
 )
import Apropos.Plutus.SingletonValue (
  SingletonValue,
  SingletonValueProp (..),
 )

import Data.List (sort)

import Test.Syd
import Test.Syd.Hedgehog

type MultiValue = [SingletonValue]

data MultiValueProp
  = HasSomeAda
  | HasSomeDana
  | HasSomeJunk
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Enumerable, Hashable)

instance LogicalModel MultiValueProp where
  logic = Yes

data PropMeaning
  = AnyElem (Formula SingletonValueProp)
  | AllElems (Formula SingletonValueProp)

propsFor :: MultiValueProp -> PropMeaning
propsFor HasSomeAda = AnyElem (All [Var $ AC IsAda, Var $ Amt IsPositive])
propsFor HasSomeDana = AnyElem (All [Var $ AC IsDana, Var $ Amt IsPositive])
propsFor HasSomeJunk = AnyElem (Var $ AC IsOther)

instance HasLogicalModel MultiValueProp MultiValue where
  satisfiesProperty (propsFor -> AnyElem props) = any $ satisfiesExpression props
  satisfiesProperty (propsFor -> AllElems props) = all $ satisfiesExpression props

instance HasPermutationGenerator MultiValueProp MultiValue where
  sources =
    [ Source
        { sourceName = "empty"
        , covers = None [Var p | p <- enumerated]
        , gen = pure []
        }
    ]
  generators =
    [ Morphism
      { name = "Add " ++ show p
      , match = Yes
      , contract = add p
      , morphism = \xs -> do
          ys <- list (linear 1 10) $ genSatisfying props
          pure $ sort $ ys ++ xs
      }
    | p <- enumerated
    , AnyElem props <- pure $ propsFor p
    ]
      ++ [ Morphism
          { name = "Remove " ++ show p
          , match = Var p
          , contract = remove p
          , morphism = return . sort . filter (not . satisfiesExpression props)
          }
         | p <- enumerated
         , AnyElem props <- pure $ propsFor p
         ]

instance HasParameterisedGenerator MultiValueProp MultiValue where
  parameterisedGenerator = buildGen

spec :: Spec
spec = do
  describe "valueGenSelfTests" $
    fromHedgehogGroup $
      permutationGeneratorSelfTest @MultiValueProp
