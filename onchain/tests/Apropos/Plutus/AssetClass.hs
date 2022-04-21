module Apropos.Plutus.AssetClass (
  AssetClassProp (..),
  spec,
  ada,
  dana,
  dusd,
  liquidityAC,
) where

import Apropos
import Control.Monad (replicateM)
import Data.Maybe (mapMaybe)
import Data.String
import GHC.Generics
import Plutus.V1.Ledger.Value
import Test.Syd
import Test.Syd.Hedgehog

data AssetClassProp
  = IsAda
  | IsDana
  | IsDUSD
  | IsLiquidity
  | IsOther
  deriving stock (Eq, Ord, Enum, Show, Bounded, Generic)
  deriving anyclass (Enumerable)

specialAC :: AssetClassProp -> Maybe AssetClass
specialAC IsAda = Just ada
specialAC IsDana = Just dana
specialAC IsDUSD = Just dusd
specialAC IsLiquidity = Just liquidityAC
specialAC IsOther = Nothing

-- Placeholder hashes
ada :: AssetClass
ada = AssetClass ("", "")

dana :: AssetClass
dana = AssetClass ("0aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa", "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")

dusd :: AssetClass
dusd = AssetClass ("1aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa", "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")

liquidityAC :: AssetClass
liquidityAC = AssetClass ("2aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa", "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")

specialTokens :: [AssetClass]
specialTokens = mapMaybe specialAC enumerated

instance LogicalModel AssetClassProp where
  logic = ExactlyOne [Var IsAda, Var IsDana, Var IsDUSD, Var IsLiquidity, Var IsOther]

instance HasLogicalModel AssetClassProp AssetClass where
  satisfiesProperty property ac = case specialAC property of
    Just ac' -> ac == ac'
    Nothing -> ac `notElem` specialTokens

instance HasPermutationGenerator AssetClassProp AssetClass where
  generators =
    Morphism
      { name = "IsOther"
      , match = Not $ Var IsOther
      , contract = clear >> add IsOther
      , morphism = \_ -> genFilter (`notElem` specialTokens) baseGen
      } :
      [ Morphism
        { name = "SetToken" ++ show property
        , match = Not $ Var property
        , contract = clear >> add property
        , morphism = \_ -> pure ac
        }
      | property <- enumerated
      , Just ac <- pure $ specialAC property
      ]

baseGen :: Gen AssetClass
baseGen =
  choice
    [ (\c -> AssetClass (fromString c, fromString c)) <$> constString
    , fmap AssetClass . (,) <$> hexString <*> hexString
    ]
  where
    hexString :: IsString s => Gen s
    hexString = fromString <$> replicateM 64 hexIt
    constString :: Gen String
    constString = replicate 64 <$> hexIt
    hexIt :: Gen Char
    hexIt = element "01234567890abcdef"

instance HasParameterisedGenerator AssetClassProp AssetClass where
  parameterisedGenerator = buildGen baseGen

spec :: Spec
spec = do
  describe "assetClassGenSelfTest" $
    mapM_ fromHedgehogGroup $
      permutationGeneratorSelfTest
        True
        (\(_ :: Morphism AssetClassProp assetClassGenSelfTest) -> True)
        baseGen
