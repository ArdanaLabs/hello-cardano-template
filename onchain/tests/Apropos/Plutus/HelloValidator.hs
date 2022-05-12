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

type HelloModel = Either Integer (Integer, Integer)

data HelloProp
  = IsValid
  | IsInvalid
  | IsMalformed
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)
  deriving anyclass (Enumerable, Hashable)

instance LogicalModel HelloProp where
  logic = ExactlyOne [Var IsValid, Var IsInvalid, Var IsMalformed]

instance HasLogicalModel HelloProp HelloModel where
  satisfiesProperty IsValid (Right (i, j)) = i + 1 == j
  satisfiesProperty IsInvalid p@(Right _) = not $ satisfiesProperty IsValid p
  satisfiesProperty IsMalformed (Left _) = True
  satisfiesProperty _ _ = False

instance HasPermutationGenerator HelloProp HelloModel where
  sources =
    [ Source
        { sourceName = "Well Formed"
        , covers = Not (Var IsMalformed)
        , gen = do
            r <-
              (,) <$> (fromIntegral <$> int (linear minBound maxBound))
                <*> (fromIntegral <$> int (linear minBound maxBound))
            pure (Right r)
        }
    , Source
        { sourceName = "Malformed"
        , covers = Var IsMalformed
        , gen = Left . fromIntegral <$> int (linear minBound maxBound)
        }
    ]
  generators =
    [ Morphism
        { name = "MakeValid"
        , match = Not $ Var IsValid
        , contract = clear >> addAll [IsValid]
        , morphism = \m -> do
            let i = helloModelInp m
            pure $ Right (i, i + 1)
        }
    , Morphism
        { name = "MakeInvalid"
        , match = Not $ Var IsInvalid
        , contract = clear >> addAll [IsInvalid]
        , morphism = \m -> do
            let i = helloModelInp m
            j <- genFilter (/= (i + 1)) (fromIntegral <$> int (linear minBound maxBound))
            pure $ Right (i, j)
        }
    ]

instance HasParameterisedGenerator HelloProp HelloModel where
  parameterisedGenerator = buildGen

mkCtx :: HelloModel -> Context
mkCtx hm = Context $ toBuiltinData scCtx
  where
    scCtx = ScriptContext txInf (Spending txInORef)
    txInf =
      TxInfo
        { txInfoInputs = [TxInInfo txInORef txInResolved]
        , txInfoOutputs = [txOutResolved]
        , txInfoFee = noValue
        , txInfoMint = noValue
        , txInfoDCert = []
        , txInfoWdrl = []
        , txInfoValidRange = undefined
        , txInfoSignatories = []
        , txInfoData = [(datumHash datumOut, datumOut)]
        , txInfoId = undefined
        }
    datumIn = mkDatum $ helloModelInp hm
    datumOut = case hm of
      Right (_, j) -> mkDatum j
      Left i -> Datum $ toBuiltinData (Just i)
    txInORef = TxOutRef txInORefId 0
    txInORefId = TxId "0000000000000000000000000000000000000000000000000000000000000000"
    txInResolved = TxOut helloAddress someAda (Just (datumHash datumIn))
    txOutResolved = TxOut helloAddress someAda (Just (datumHash datumOut))

mkDatum :: Integer -> Datum
mkDatum i = Datum $ toBuiltinData i

helloModelInp :: HelloModel -> Integer
helloModelInp (Right (i, _)) = i
helloModelInp (Left i) = i

someAda :: Value
someAda = Value (fromList [(currencySymbol "", fromList [(tokenName "", 10)])])

noValue :: Value
noValue = Value (fromList [])

instance ScriptModel HelloProp HelloModel where
  expect = Var IsValid
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
