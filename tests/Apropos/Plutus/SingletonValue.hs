module Apropos.Plutus.SingletonValue (
    spec,
    SingletonValue,
    SingletonValueProp (..),
) where

import Apropos
import Apropos.Plutus.AssetClass (AssetClassProp)
import Apropos.Plutus.Integer (IntegerProp)
import Control.Lens
import Control.Monad (join)
import Plutus.V1.Ledger.Value (AssetClass)

import Test.Syd
import Test.Syd.Hedgehog

type SingletonValue = (AssetClass, Integer)

data SingletonValueProp
    = AC AssetClassProp
    | Amt IntegerProp
    deriving stock (Eq, Ord, Show)

instance Enumerable SingletonValueProp where
    enumerated = (AC <$> enumerated) <> (Amt <$> enumerated)

-- TemplateHaskell breaks my hls so I wrote it by hand for now

instance LogicalModel SingletonValueProp where
    logic = (AC <$> logic) :&&: (Amt <$> logic)

instance HasLogicalModel SingletonValueProp SingletonValue where
    satisfiesProperty (AC p) (ac, _) = satisfiesProperty p ac
    satisfiesProperty (Amt p) (_, amt) = satisfiesProperty p amt

instance HasPermutationGenerator SingletonValueProp SingletonValue where
    generators =
        let l =
                Abstraction
                    { abstractionName = "assetClass"
                    , propertyAbstraction = abstractsProperties AC
                    , modelAbstraction = _1
                    }
            r =
                Abstraction
                    { abstractionName = "amt"
                    , propertyAbstraction = abstractsProperties Amt
                    , modelAbstraction = _2
                    }
         in join [abstract l <$> generators, abstract r <$> generators]

instance HasParameterisedGenerator SingletonValueProp SingletonValue where
    parameterisedGenerator = buildGen baseGen

baseGen :: Gen SingletonValue
baseGen =
    (,) <$> genSatisfying @AssetClassProp Yes
        <*> genSatisfying @IntegerProp Yes

spec :: Spec
spec = do
    describe "singletonValueGenSelfTests" $
        mapM_ fromHedgehogGroup $
            permutationGeneratorSelfTest
                True
                (\(_ :: Morphism SingletonValueProp singletonValueGenSelfTests) -> True)
                baseGen
