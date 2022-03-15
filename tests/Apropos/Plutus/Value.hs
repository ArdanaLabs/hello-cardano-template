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
    deriving stock (Eq, Ord, Show, Enum, Bounded)

instance Enumerable MultiValueProp where
    enumerated = [minBound .. maxBound]

instance LogicalModel MultiValueProp where
    logic = Yes

data PropMeaning
    = Any (Formula SingletonValueProp)
    | Every (Formula SingletonValueProp)

propsFor :: MultiValueProp -> PropMeaning
propsFor HasSomeAda = Any (All [Var $ AC IsAda, Var $ Amt IsPositive])
propsFor HasSomeDana = Any (All [Var $ AC IsDana, Var $ Amt IsPositive])
propsFor HasSomeJunk = Any (Var $ AC IsOther)

instance HasLogicalModel MultiValueProp MultiValue where
    satisfiesProperty (propsFor -> Any props) = any $ satisfiesExpression props
    satisfiesProperty (propsFor -> Every props) = all $ satisfiesExpression props

instance HasPermutationGenerator MultiValueProp MultiValue where
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
        , Any props <- pure $ propsFor p
        ]
            ++ [ Morphism
                { name = "Remove " ++ show p
                , match = Var p
                , contract = remove p
                , morphism = return . sort . filter (not . satisfiesExpression props)
                }
               | p <- enumerated
               , Any props <- pure $ propsFor p
               ]

instance HasParameterisedGenerator MultiValueProp MultiValue where
    parameterisedGenerator = buildGen $ pure []

-- TODO better base gen?

spec :: Spec
spec = do
    describe "valueGenSelfTests" $
        mapM_ fromHedgehogGroup $
            permutationGeneratorSelfTest
                True
                (const @Bool @(Morphism MultiValueProp MultiValue) True)
                (pure [])
