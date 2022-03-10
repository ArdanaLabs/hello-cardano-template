module Main ( main ) where
import qualified Apropos.Plutus.SingletonValue as SingletonValue
import Test.Tasty

main :: IO ()
main =
  defaultMain $ testGroup "Plutus" [
    SingletonValue.singletonValueGenSelfTests
                ]
