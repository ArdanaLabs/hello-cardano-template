module Apropos.Plutus.Hello (
  HelloProp (..),
  spec,
) where

import Apropos

import Test.Syd
import Test.Syd.Hedgehog

import Plutarch (compile, (#))
import Plutarch.Evaluate (evalScript)
import Plutarch.Prelude qualified as PPrelude

import Hello (helloLogic)

type HelloModel = (Integer, Integer)

data HelloProp
  = IsValid
  | IsInvalid
  deriving stock (Show, Eq, Ord, Enum, Bounded)

instance Enumerable HelloProp where
  enumerated = [minBound .. maxBound]

instance LogicalModel HelloProp where
  logic = ExactlyOne [Var IsValid, Var IsInvalid]

instance HasLogicalModel HelloProp HelloModel where
  satisfiesProperty IsValid (i, j) = i + 1 == j
  satisfiesProperty IsInvalid p = not $ satisfiesProperty IsValid p

instance HasPermutationGenerator HelloProp HelloModel where
  generators =
    [ Morphism
        { name = "MakeValid"
        , match = Not $ Var IsValid
        , contract = clear >> addAll [IsValid]
        , morphism = \(i, _j) -> pure (i, i + 1)
        }
    , Morphism
        { name = "MakeInvalid"
        , match = Not $ Var IsInvalid
        , contract = clear >> addAll [IsInvalid]
        , morphism = \(i, j) -> pure (i, j + 1)
        }
    ]

instance HasParameterisedGenerator HelloProp HelloModel where
  parameterisedGenerator =
    buildGen $
      (,)
        <$> (fromIntegral <$> int (linear (-10) 10))
        <*> (fromIntegral <$> int (linear (-10) 10))

instance HasPureRunner HelloProp HelloModel where
  expect _ = Var IsValid
  script _ (i, j) = helloLogic' i j

helloLogic' :: Integer -> Integer -> Bool
helloLogic' i j = evaledScript == Right compiledUnit
  where
    compiledUnit = compile (PPrelude.pconstant ())

    (evaledScript, _, _) = evalScript $ compile $ helloLogic # PPrelude.pconstant i # PPrelude.pconstant j

spec :: Spec
spec = do
  describe "helloGenSelfTest" $
    mapM_ fromHedgehogGroup $
      [ runGeneratorTestsWhere (Apropos :: HelloModel :+ HelloProp) "Hello Generator" Yes
      ]
  describe "helloPureTests" $
    mapM_ fromHedgehogGroup $
      [ runPureTestsWhere (Apropos :: HelloModel :+ HelloProp) "AcceptsValid" Yes
      ]
