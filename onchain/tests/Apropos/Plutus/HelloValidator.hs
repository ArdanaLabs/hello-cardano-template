module Apropos.Plutus.HelloValidator (
  HelloProp (..),
  spec,
) where

import Apropos
import Apropos.Script
import Apropos.ContextBuilder

import Test.Syd hiding (Context)
import Test.Syd.Hedgehog

import Plutus.V1.Ledger.Api (
  Redeemer (..),
  Value (..),
  toBuiltinData,
 )
import Plutus.V1.Ledger.Scripts (Context (..), Datum (..), applyValidator)
import Plutus.V1.Ledger.Value (currencySymbol, tokenName)
import Plutus.V2.Ledger.Api (fromList)

import Hello (helloAddress, helloValidator)

type HelloModel = (Bool, Integer, Integer)

data HelloProp
  = IsValid
  | IsInvalid
  | IsMalformed
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)
  deriving anyclass (Enumerable, Hashable)

instance LogicalModel HelloProp where
  logic = ExactlyOne [Var IsValid, Var IsInvalid]

instance HasLogicalModel HelloProp HelloModel where
  satisfiesProperty IsValid (_, i, j) = i + 1 == j
  satisfiesProperty IsInvalid p = not $ satisfiesProperty IsValid p
  satisfiesProperty IsMalformed (b, _, _) = b

instance HasPermutationGenerator HelloProp HelloModel where
  sources =
    [ Source
        { sourceName = "Yes"
        , covers = Yes
        , gen =
            (,,) <$> bool
              <*> (fromIntegral <$> int (linear minBound maxBound))
              <*> (fromIntegral <$> int (linear minBound maxBound))
        }
    ]
  generators =
    [ Morphism
        { name = "MakeValid"
        , match = Not $ Var IsValid
        , contract = swap IsValid IsInvalid
        , morphism = \(m, i, _) -> pure (m, i, i + 1)
        }
    , Morphism
        { name = "MakeInvalid"
        , match = Not $ Var IsInvalid
        , contract = swap IsInvalid IsValid
        , morphism = \(m, i, _) -> do
            j <- genFilter (/= (i + 1)) (fromIntegral <$> int (linear minBound maxBound))
            pure (m, i, j)
        }
    , Morphism
        { name = "ToggleMalformed"
        , match = Yes
        , contract = toggle IsMalformed
        , morphism = \(m, i, j) -> pure (not m, i, j)
        }
    ]

instance HasParameterisedGenerator HelloProp HelloModel where
  parameterisedGenerator = buildGen

mkCtx :: HelloModel -> Context
mkCtx (m, i, j) =
  buildContext $ do
    withTxInfo $ do
      addInput nullTxOutRef helloAddress someAda (Just datumIn)
      addOutput helloAddress someAda (Just datumOut)

      txInfoIdUntouched
      txInfoSignatoriesUntouched
      txInfoValidRangeUntouched
      txInfoWdrlUntouched
      txInfoDCertUntouched
      txInfoMintUntouched
      txInfoFeeUntouched
  where
    datumIn = Datum $ toBuiltinData i
    datumOut =
      Datum $
        if m
          then toBuiltinData $ Just i
          else toBuiltinData j
    someAda = Value (fromList [(currencySymbol "", fromList [(tokenName "", 10)])])

instance ScriptModel HelloProp HelloModel where
  expect = Var IsValid :&&: Not (Var IsMalformed)
  script hm@(_,i,_) = applyValidator (mkCtx hm) helloValidator (Datum (toBuiltinData i)) (Redeemer (toBuiltinData ()))

spec :: Spec
spec = do
  describe "helloValidatorGenSelfTest" $
    mapM_
      fromHedgehogGroup
      [ runGeneratorTestsWhere @HelloProp "Hello Generator" Yes
      ]
  describe "helloValidatorTests" $
    mapM_
      fromHedgehogGroup
      [ runScriptTestsWhere @HelloProp "AcceptsValid" Yes
      ]
