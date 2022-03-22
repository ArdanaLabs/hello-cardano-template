module Apropos.Plutus.Integer (
    IntegerProp (..),
    spec,
) where

import Apropos

import Test.Syd
import Test.Syd.Hedgehog

data IntegerProp
    = IsNegative
    | IsPositive
    | IsZero
    | IsLarge
    | IsSmall
    deriving stock (Show, Eq, Ord, Enum, Bounded)

instance Enumerable IntegerProp where
    enumerated = [minBound .. maxBound]

instance LogicalModel IntegerProp where
    logic =
        ExactlyOne [Var IsNegative, Var IsPositive, Var IsZero]
            :&&: ExactlyOne [Var IsLarge, Var IsSmall]
            :&&: (Var IsZero :->: Var IsSmall)

instance HasLogicalModel IntegerProp Integer where
    satisfiesProperty IsNegative i = i < 0
    satisfiesProperty IsPositive i = i > 0
    satisfiesProperty IsZero i = i == 0
    satisfiesProperty IsLarge i = i > 10 || i < -10
    satisfiesProperty IsSmall i = i <= 10 && i >= -10

instance HasPermutationGenerator IntegerProp Integer where
    generators =
        [ Morphism
            { name = "MakeZero"
            , match = Not $ Var IsZero
            , contract = clear >> addAll [IsZero, IsSmall]
            , morphism = \_ -> pure 0
            }
        , Morphism
            { name = "MakeLarge"
            , match = Not $ Var IsLarge
            , contract = clear >> addAll [IsLarge, IsPositive]
            , morphism = \_ -> do
                i <- int (linear 11 (maxBound -1))
                pure $ fromIntegral i
            }
        , Morphism
            { name = "MakeSmall"
            , match = Not $ Var IsSmall
            , contract = clear >> addAll [IsSmall, IsPositive]
            , morphism = \_ -> do
                i <- int (linear 1 10)
                pure $ fromIntegral i
            }
        , Morphism
            { name = "Negate"
            , match = Not $ Var IsZero
            , contract =
                branches
                    [ has IsNegative >> remove IsNegative >> add IsPositive
                    , has IsPositive >> remove IsPositive >> add IsNegative
                    ]
            , morphism = pure . negate
            }
        ]

instance HasParameterisedGenerator IntegerProp Integer where
    parameterisedGenerator = buildGen $ pure 0

spec :: Spec
spec = do
    describe "integerGenSelfTest" $
        mapM_ fromHedgehogGroup $
            permutationGeneratorSelfTest
                True
                (\(_ :: Morphism IntegerProp integerGenSelfTest) -> True)
                (pure (0 :: Integer))
