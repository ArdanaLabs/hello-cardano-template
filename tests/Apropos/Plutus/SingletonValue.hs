module Apropos.Plutus.SingletonValue (
  singletonValueGenSelfTests,
  SingletonValue,
  SingletonValueProp,
  ) where
import Apropos
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (fromGroup)
import Control.Monad ( replicateM )
import Data.String (IsString(..))
import Apropos.Plutus.List
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
  | IsOther
  deriving stock (Eq, Ord, Enum, Show, Bounded)

instance Enumerable SingletonValueProp where
  enumerated = [minBound .. maxBound]

instance LogicalModel SingletonValueProp where
  logic =
    ExactlyOne [Var IsNegative, Var IsPositive, Var IsZero]
      :&&: ExactlyOne [Var IsLarge, Var IsSmall]
      :&&: ExactlyOne [Var IsAda,Var IsOther]
      :&&: (Var IsZero :->: Var IsSmall)

instance Satable SingletonValueProp where
  sats =  [IsNegative,IsLarge,IsAda]

instance HasLogicalModel SingletonValueProp SingletonValue where
  satisfiesProperty IsNegative (_,_,i) = i < 0
  satisfiesProperty IsPositive (_,_,i) = i > 0
  satisfiesProperty IsZero     (_,_,i) = i == 0
  satisfiesProperty IsLarge    (_,_,i) = i > 10 || i < -10
  satisfiesProperty IsSmall    (_,_,i) = i <= 10 && i >= -10
  satisfiesProperty IsAda      (c,t,_) = (c,t) == ("","")
  satisfiesProperty IsOther    (c,t,_) = (c,t) /= ("","")

instance HasPermutationGenerator SingletonValueProp SingletonValue where
  generators =
    [ Morphism
        { name = "MakeZero"
        , match = Not $ Var IsZero
        , contract = clear >> addAll [IsZero, IsSmall] >> addIf IsAda IsAda
        , morphism = \(c,t,_) -> pure (c,t,0)
        }
    , Morphism
        { name = "MakeLarge"
        , match = Not $ Var IsLarge
        , contract = clear >> addAll [IsLarge, IsPositive] >> addIf IsAda IsAda
        , morphism = \(c,t,_) -> do
            i <- int (linear 11 (maxBound -1))
            pure (c,t,fromIntegral i)
        }
    , Morphism
        { name = "MakeSmall"
        , match = Not $ Var IsSmall
        -- TODO filtered clear might be nicer than addIf
        , contract = clear >> addAll [IsSmall, IsPositive] >> addIf IsAda IsAda
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
    , Morphism
      { name = "Adafy"
      , match = Not $ Var IsAda
      , contract = remove IsOther >> add IsAda
      -- TODO once other currencies have props this will need to remove them
      -- if we're going to need to distinguish amount properties
      -- from asset properties should we just model this as
      -- a pair of an assetClasses and an integer?
      , morphism = \(_,_,i) -> pure ("","",i)
      }
    , Morphism
      { name = "UnAdafy"
      , match = Var IsAda
      , contract = remove IsAda >> add IsOther
      , morphism = \(_,_,i) -> do
          cs <- nonAdaName
          tn <- nonAdaName
          pure (cs,tn,i)
      }
    ]

nonAdaName :: IsString s => Gen s
nonAdaName = fromString <$> replicateM 64 hexit
  where
    hexit = element $ ['0'..'9'] ++ ['a'..'f']

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

