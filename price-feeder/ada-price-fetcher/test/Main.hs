module Main where

import System.Process (proc, readCreateProcess)
import Test.Syd

main :: IO ()
main = sydTest $ do
  spec

execAdaPriceFetcher :: Int -> IO Double
execAdaPriceFetcher minNumAdaPrices =
  read <$> readCreateProcess (proc "ada-price-fetcher" ["-N", show minNumAdaPrices]) ""

spec :: Spec
spec = do
  describe "ada-price-fetcher" $ do
    it "should return the Ada price median based on at least 2 prices." $ do
      execAdaPriceFetcher 2 >>= (`shouldSatisfy` (> 0.0))
    it "should throw if it wasn't able to fetch the minimal number of prices" $ do
      execAdaPriceFetcher 200 `shouldThrow` anyException
