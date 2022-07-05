module Main where

import Test.Syd
import System.Process (proc, readCreateProcess)

main :: IO ()
main = sydTest $ do
  spec

execAdaPriceFeeder :: Int -> IO Double
execAdaPriceFeeder minNumAdaPrices =
  read <$> readCreateProcess (proc "ada-price-feeder" ["-N", show minNumAdaPrices]) ""

spec :: Spec
spec = do
  describe "ada-price-feeder" $ do
    it "should return the Ada price median based on at least 2 prices." $ do
      execAdaPriceFeeder 2 >>= (`shouldSatisfy` (> 0.0))
    it "should throw if it wasn't able to fetch the minimal number of prices" $ do
      execAdaPriceFeeder 200 `shouldThrow` anyException
