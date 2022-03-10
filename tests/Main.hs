module Main ( main ) where
import qualified Apropos.Plutus.SingletonValue as SingletonValue
import Test.Tasty

--import qualified Models.Vault as Vault (spec)
--import qualified Models.Auction as Auction (spec)
--import Test.Syd (sydTest)
import qualified Models.Vault as Vault (hedge)
import Test.Tasty.Hedgehog ( fromGroup )

-- TODO should we use sydtest-discover?
main :: IO ()
main =
  defaultMain $ testGroup "Plutus"
    [ SingletonValue.singletonValueGenSelfTests
    , fromGroup Vault.hedge
                ]
