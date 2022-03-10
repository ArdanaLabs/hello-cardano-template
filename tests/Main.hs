module Main ( main ) where

--import qualified Models.Vault as Vault (spec)
--import qualified Models.Auction as Auction (spec)
--import Test.Syd (sydTest)
import qualified Models.Vault as Vault (hedge)
import Hedgehog
import Control.Monad (void)

-- TODO should we use sydtest-discover?
main :: IO ()
main = void $ checkParallel Vault.hedge
--  sydTest $ do
--  Vault.spec
--  Auction.spec
