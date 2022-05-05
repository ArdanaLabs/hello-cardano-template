{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}
module MyLib (getMedianPriceFromSources) where

import Data.Either (fromRight)
import Data.Text qualified as T
import Data.Vector qualified as V (fromList)
import Statistics.Quantile (median, medianUnbiased)
import Servant.Client (BaseUrl, ClientEnv, mkClientEnv)
import Network.HTTP.Client.TLS (newTlsManager)

import Network.Binance.Client (binanceBaseUrl, getPrice)
import Network.Coinbase.Client (coinbaseBaseUrl, getSpotPrice)
import Network.Huobi.Client (huobiBaseUrl, getTick)
import Network.Kraken.Client (krakenBaseUrl, getOHLCData)
import Network.Kucoin.Client (kucoinBaseUrl, getFiatPrice)
import PriceData.Types (CurrencyPair(..))

adaUsd :: CurrencyPair
adaUsd = CurrencyPair {
    _base = "ADA"
  , _currency = "USD"
  }

adaUsdt :: CurrencyPair
adaUsdt = CurrencyPair {
    _base = "ADA"
  , _currency = "USDT"
  }

getBinancePrice :: (BaseUrl -> ClientEnv) -> IO (Either T.Text Double)
getBinancePrice env = do
  _ <- getPrice (env binanceBaseUrl) adaUsdt
  undefined

getCoinbasePrice :: (BaseUrl -> ClientEnv) -> IO (Either T.Text Double)
getCoinbasePrice env = do
  _ <- getSpotPrice (env coinbaseBaseUrl) adaUsd Nothing
  undefined

getHuobiPrice :: (BaseUrl -> ClientEnv) -> IO (Either T.Text Double)
getHuobiPrice env = do
  _ <- getTick (env huobiBaseUrl) adaUsdt
  undefined

getKrakenPrice :: (BaseUrl -> ClientEnv) -> IO (Either T.Text Double)
getKrakenPrice env = do
  _ <- getOHLCData (env krakenBaseUrl) adaUsd Nothing Nothing
  undefined

getKucoinPrice :: (BaseUrl -> ClientEnv) -> IO (Either T.Text Double)
getKucoinPrice env = do
  _ <- getFiatPrice (env kucoinBaseUrl) adaUsd
  undefined

-- see catMaybes

getMedianPriceFromSources :: IO Double
getMedianPriceFromSources = do
  manager <- newTlsManager
  let clientEnv = mkClientEnv manager
      fetchers :: [IO (Either T.Text Double)]
      fetchers = ($ clientEnv) <$> [getBinancePrice, getCoinbasePrice, getHuobiPrice, getKrakenPrice, getKucoinPrice]
  results <- (fmap $ fromRight 1) <$> sequenceA fetchers
  return $ median medianUnbiased $ V.fromList results

