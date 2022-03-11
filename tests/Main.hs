module Main ( main ) where
import qualified Apropos.Plutus.SingletonValue as SingletonValue
--import qualified Apropos.Plutus.Value as Value
import Test.Tasty ( defaultMain, testGroup )
import Main.Utf8 (withUtf8)

-- TODO should we use sydtest-discover?
main :: IO ()
main = withUtf8 $ -- TODO this seems to break the histograms a bit
  defaultMain $ testGroup "Plutus"
    [ SingletonValue.singletonValueGenSelfTests
    --, Value.valueGenSelfTests
                ]
