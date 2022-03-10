module Apropos.Plutus.SingletonValue (
  singletonValueGenSelfTests,
  ) where
import Apropos
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (fromGroup)
import Plutus.V1.Ledger.Api
  ( CurrencySymbol
  , TokenName
  )

type SingletonValue = (CurrencySymbol,TokenName,Integer)

data SingletonValueProp
  = IsNegative
  | IsPositive
  | IsZero
  | IsLarge
  | IsSmall
  | IsAda
--  | IsOther
  deriving stock (Eq, Ord, Enum, Show, Bounded)

instance Enumerable SingletonValueProp where
  enumerated = [minBound .. maxBound]

instance LogicalModel SingletonValueProp where
  logic =
    ExactlyOne [Var IsNegative, Var IsPositive, Var IsZero]
      :&&: ExactlyOne [Var IsLarge, Var IsSmall]
      :&&: (Var IsZero :->: Var IsSmall)

instance HasLogicalModel SingletonValueProp SingletonValue where
  satisfiesProperty IsNegative (_,_,i) = i < 0
  satisfiesProperty IsPositive (_,_,i) = i > 0
  satisfiesProperty IsZero     (_,_,i) = i == 0
  satisfiesProperty IsLarge    (_,_,i) = i > 10 || i < -10
  satisfiesProperty IsSmall    (_,_,i) = i <= 10 && i >= -10
  satisfiesProperty IsAda      (c,t,_) = c == "" && t == ""

instance HasPermutationGenerator SingletonValueProp SingletonValue where
  generators =
    [ Morphism
        { name = "MakeZero"
        , match = Not $ Var IsZero
        , contract = clear >> addAll [IsZero, IsSmall]
        , morphism = \(c,t,_) -> pure (c,t,0)
        }
    , Morphism
        { name = "MakeLarge"
        , match = Not $ Var IsLarge
        , contract = clear >> addAll [IsLarge, IsPositive]
        , morphism = \(c,t,_) -> do
            i <- int (linear 11 (maxBound -1))
            pure (c,t,fromIntegral i)
        }
    , Morphism
        { name = "MakeSmall"
        , match = Not $ Var IsSmall
        , contract = clear >> addAll [IsSmall, IsPositive]
        , morphism = \(c,t,_) -> do
            i <- int (linear 1 10)
            pure (c,t,fromIntegral i)
        }
    , Morphism
        { name = "Negate"
        , match = Not $ Var IsZero
        , contract =
            branches
              [ has IsNegative >> remove IsNegative >> add IsPositive
              , has IsPositive >> remove IsPositive >> add IsNegative
              ]
        , morphism = \(c,t,i) -> pure (c,t,-i)
        }
    ]

instance HasParameterisedGenerator SingletonValueProp SingletonValue where
  parameterisedGenerator = buildGen baseGen

baseGen :: Gen SingletonValue
baseGen = do
  i <- int (linear minBound maxBound)
  pure ("","",fromIntegral i)

singletonValueGenSelfTests :: TestTree
singletonValueGenSelfTests =
  testGroup "singletonValueGenSelfTests" $
    fromGroup
      <$> permutationGeneratorSelfTest
        True
        (\(_ :: Morphism SingletonValueProp singletonValueGenSelfTests) -> True)
        baseGen

