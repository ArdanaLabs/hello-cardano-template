module Main (main) where

import qualified Apropos.Plutus.SingletonValue as SingletonValue
import qualified Apropos.Plutus.AssetClass as AssetClass
import qualified Apropos.Plutus.Integer as Integer
import qualified Apropos.Plutus.Value as Value

import Test.Tasty ( defaultMain, testGroup )
import Main.Utf8 (withUtf8)

-- TODO use sydtest-discover?
main :: IO ()
main = withUtf8 $ do -- TODO this seems to break the histograms a bit
  defaultMain $ testGroup "Plutus"
    [ AssetClass.assetClassGenSelfTest
    , Integer.integerGenSelfTest
    , SingletonValue.singletonValueGenSelfTests
    , Value.valueGenSelfTests
    ]
