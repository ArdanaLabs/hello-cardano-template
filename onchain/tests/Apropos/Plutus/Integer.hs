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
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Enumerable, Hashable)

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
  sources =
    [ Source
        { sourceName = "Zero"
        , covers = Var IsZero
        , gen = pure 0
        }
    , Source
        { sourceName = "Large"
        , covers = Var IsPositive :&&: Var IsLarge
        , gen = fromIntegral <$> int (linear 11 maxBound)
        }
    , Source
        { sourceName = "Small"
        , covers = Var IsPositive :&&: Var IsSmall
        , gen = fromIntegral <$> int (linear 1 10)
        }
    ]
  generators =
    [ Morphism
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
  parameterisedGenerator = buildGen

spec :: Spec
spec = do
  describe "integerGenSelfTest" $
    fromHedgehogGroup $
      permutationGeneratorSelfTest @IntegerProp
