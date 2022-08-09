module Apropos.Plutus.SingletonValue (
  spec,
  SingletonValue,
  SingletonValueProp (..),
) where

import Apropos
import Apropos.Plutus.AssetClass (AssetClassProp)
import Apropos.Plutus.Integer (IntegerProp)

{- HLINT ignore Avoid restricted module -}
import Control.Lens
import Plutus.V1.Ledger.Value (AssetClass)

import Test.Syd
import Test.Syd.Hedgehog

type SingletonValue = (AssetClass, Integer)

data SingletonValueProp
  = AC AssetClassProp
  | Amt IntegerProp
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Enumerable, Hashable)

instance LogicalModel SingletonValueProp where
  logic = abstractionLogic @SingletonValue

instance HasLogicalModel SingletonValueProp SingletonValue where
  satisfiesProperty (AC p) (ac, _) = satisfiesProperty p ac
  satisfiesProperty (Amt p) (_, amt) = satisfiesProperty p amt

instance HasAbstractions SingletonValueProp SingletonValue where
  sourceAbstractions =
    [ SoAs $
        SourceAbstraction
          { sourceAbsName = "(,)"
          , constructor = (,)
          , productAbs =
              ProductAbstraction
                { abstractionName = "assetClass"
                , propertyAbstraction = abstractsProperties AC
                , productModelAbstraction = _1
                }
                :& ProductAbstraction
                  { abstractionName = "amt"
                  , propertyAbstraction = abstractsProperties Amt
                  , productModelAbstraction = _2
                  }
                :& Nil
          }
    ]

instance HasPermutationGenerator SingletonValueProp SingletonValue where
  sources = abstractionSources
  generators = abstractionMorphisms

instance HasParameterisedGenerator SingletonValueProp SingletonValue where
  parameterisedGenerator = buildGen

spec :: Spec
spec = do
  describe "singletonValueGenSelfTests" $
    fromHedgehogGroup $
      permutationGeneratorSelfTest @SingletonValueProp
