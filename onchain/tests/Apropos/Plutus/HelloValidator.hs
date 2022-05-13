module Apropos.Plutus.HelloValidator (
  HelloProp (..),
  spec,
) where

import Apropos
import Apropos.ContextBuilder
import Apropos.Script

import Test.Syd hiding (Context)
import Test.Syd.Hedgehog

import Plutus.V1.Ledger.Address (pubKeyHashAddress)
import Plutus.V1.Ledger.Api (
  Redeemer (..),
  Value (..),
  toBuiltinData,
 )
import Plutus.V1.Ledger.Scripts (Context (..), Datum (..), applyValidator)
import Plutus.V1.Ledger.Value (currencySymbol, tokenName)
import Plutus.V2.Ledger.Api (fromList)

import Hello (helloAddress, helloValidator)

data HelloModel = HelloModel
  { isContinuing :: Bool
  , isMalformed :: Bool
  , isUnitRedeemer :: Bool
  , inDatum :: Integer
  , outDatum :: Integer
  }
  deriving stock (Show)

data HelloProp
  = IsValid
  | IsInvalid
  | IsMalformed
  | IsContinuing
  | IsUnitRedeemer
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)
  deriving anyclass (Enumerable, Hashable)

instance LogicalModel HelloProp where
  logic = ExactlyOne [Var IsValid, Var IsInvalid]

instance HasLogicalModel HelloProp HelloModel where
  satisfiesProperty IsValid HelloModel {..} = inDatum + 1 == outDatum
  satisfiesProperty IsInvalid p = not $ satisfiesProperty IsValid p
  satisfiesProperty IsMalformed HelloModel {..} = isMalformed
  satisfiesProperty IsContinuing HelloModel {..} = isContinuing
  satisfiesProperty IsUnitRedeemer HelloModel {..} = isUnitRedeemer

instance HasPermutationGenerator HelloProp HelloModel where
  sources =
    [ Source
        { sourceName = "Yes"
        , covers = Yes
        , gen =
            HelloModel <$> bool
              <*> bool
              <*> bool
              <*> (fromIntegral <$> int (linear minBound maxBound))
              <*> (fromIntegral <$> int (linear minBound maxBound))
        }
    ]
  generators =
    [ Morphism
        { name = "MakeValid"
        , match = Not $ Var IsValid
        , contract = swap IsValid IsInvalid
        , morphism = \hm@HelloModel {..} -> pure hm {outDatum = inDatum + 1}
        }
    , Morphism
        { name = "MakeInvalid"
        , match = Not $ Var IsInvalid
        , contract = swap IsInvalid IsValid
        , morphism = \hm@HelloModel {..} -> do
            j <- genFilter (/= (inDatum + 1)) (fromIntegral <$> int (linear minBound maxBound))
            pure hm {outDatum = j}
        }
    , Morphism
        { name = "ToggleMalformed"
        , match = Yes
        , contract = toggle IsMalformed
        , morphism = \hm@HelloModel {..} -> pure hm {isMalformed = not isMalformed}
        }
    , Morphism
        { name = "ToggleContinuing"
        , match = Yes
        , contract = toggle IsContinuing
        , morphism = \hm@HelloModel {..} -> pure hm {isContinuing = not isContinuing}
        }
    , Morphism
        { name = "ToggleIsUnitRedeemer"
        , match = Yes
        , contract = toggle IsUnitRedeemer
        , morphism = \hm@HelloModel {..} -> pure hm {isUnitRedeemer = not isUnitRedeemer}
        }
    ]

instance HasParameterisedGenerator HelloProp HelloModel where
  parameterisedGenerator = buildGen

mkCtx :: HelloModel -> Context
mkCtx HelloModel {..} =
  buildContext $ do
    withTxInfo $ do
      addInput nullTxOutRef helloAddress someAda (Just datumIn)
      addOutput outAddr someAda (Just datumOut)

      txInfoIdUntouched
      txInfoSignatoriesUntouched
      txInfoValidRangeUntouched
      txInfoWdrlUntouched
      txInfoDCertUntouched
      txInfoMintUntouched
      txInfoFeeUntouched
  where
    outAddr = if isContinuing then helloAddress else pubKeyHashAddress ""
    datumIn = Datum $ toBuiltinData inDatum
    datumOut =
      Datum $
        if isMalformed
          then toBuiltinData $ Just outDatum
          else toBuiltinData outDatum
    someAda = Value (fromList [(currencySymbol "", fromList [(tokenName "", 10)])])

instance ScriptModel HelloProp HelloModel where
  expect = Var IsValid :&&: Not (Var IsMalformed) :&&: Var IsContinuing
  script hm@HelloModel {..} =
    let redeemer = Redeemer $ if isUnitRedeemer then toBuiltinData () else toBuiltinData (42 :: Integer)
     in applyValidator (mkCtx hm) helloValidator (Datum (toBuiltinData inDatum)) redeemer

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
