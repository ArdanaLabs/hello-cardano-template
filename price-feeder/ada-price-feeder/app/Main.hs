module Main where

import PriceFetcher (getMedianPriceFromSources)

main :: IO ()
main = do
  putStrLn =<< getMedianPriceFromSources
