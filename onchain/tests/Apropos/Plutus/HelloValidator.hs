module Apropos.Plutus.HelloValidator (
  HelloProp (..),
  spec,
) where

import Apropos
import Apropos.Script

import Plutarch.Api.V1 (datumHash)
import Test.Syd hiding (Context)
import Test.Syd.Hedgehog

import Plutus.V1.Ledger.Api (
  Redeemer (..),
  ScriptContext (..),
  ScriptPurpose (..),
  TxId (..),
  TxInInfo (..),
  TxInfo (..),
  TxOut (..),
  TxOutRef (..),
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
        , contract = add IsValid >> remove IsInvalid
        , morphism = \(m, i, _) -> pure (m, i, i + 1)
        }
    , Morphism
        { name = "MakeInvalid"
        , match = Not $ Var IsInvalid
        , contract = add IsInvalid >> remove IsValid
        , morphism = \(m, i, _) -> do
            j <- genFilter (/= (i + 1)) (fromIntegral <$> int (linear minBound maxBound))
            pure (m, i, j)
        }
    , Morphism
        { name = "ToggleMalformed"
        , match = Yes
        , contract =
            branches
              [ has IsMalformed >> remove IsMalformed
              , hasn't IsMalformed >> add IsMalformed
              ]
        , morphism = \(m, i, j) -> pure (not m, i, j)
        }
    ]

instance HasParameterisedGenerator HelloProp HelloModel where
  parameterisedGenerator = buildGen

untouched :: a
untouched = error "untouched by script"

mkCtx :: HelloModel -> Context
mkCtx hm@(m, i, j) = Context $ toBuiltinData scCtx
  where
    scCtx = ScriptContext txInf (Spending txInORef)
    txInf =
      TxInfo
        { txInfoInputs = [TxInInfo txInORef txInResolved]
        , txInfoOutputs = [txOutResolved]
        , txInfoFee = untouched
        , txInfoMint = untouched
        , txInfoDCert = untouched
        , txInfoWdrl = untouched
        , txInfoValidRange = untouched
        , txInfoSignatories = untouched
        , txInfoData = [(datumHash datumOut, datumOut)]
        , txInfoId = untouched
        }
    datumIn = mkDatum $ helloModelInp hm
    datumOut =
      Datum $
        if m
          then toBuiltinData $ Just i
          else toBuiltinData j
    txInORef = TxOutRef txInORefId 0
    txInORefId = TxId "0000000000000000000000000000000000000000000000000000000000000000"
    txInResolved = TxOut helloAddress someAda (Just (datumHash datumIn))
    txOutResolved = TxOut helloAddress someAda (Just (datumHash datumOut))

mkDatum :: Integer -> Datum
mkDatum i = Datum $ toBuiltinData i

helloModelInp :: HelloModel -> Integer
helloModelInp (_, i, _) = i

someAda :: Value
someAda = Value (fromList [(currencySymbol "", fromList [(tokenName "", 10)])])

instance ScriptModel HelloProp HelloModel where
  expect = Var IsValid :&&: Not (Var IsMalformed)
  script hm = applyValidator (mkCtx hm) helloValidator (mkDatum (helloModelInp hm)) (Redeemer (toBuiltinData ()))

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
