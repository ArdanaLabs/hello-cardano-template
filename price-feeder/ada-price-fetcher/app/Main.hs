module Main where

import Options.Applicative

import PriceFetcher (getMedianPriceFromSources)

main :: IO ()
main = do
  n <- execParser priceFetcherOptions
  print =<< getMedianPriceFromSources n

priceFetcherOptions :: ParserInfo Int
priceFetcherOptions =
  info
    (minNumberOfPrices <**> helper)
    ( fullDesc
        <> progDesc "Fetches the current Ada price from Binance, Coinbase, Huobi, Kraken and Kukoin and computes the median"
        <> header "ada-price-fetcher"
    )

minNumberOfPrices :: Parser Int
minNumberOfPrices =
  option
    auto
    ( long "min-prices"
        <> short 'N'
        <> metavar "INT"
        <> help "The minimal number of prices that needs to be fetched succesfully for the program to succeed"
        <> value 5
        <> showDefault
    )
