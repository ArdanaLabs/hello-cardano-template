{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}
module MyLib (getMedianPriceFromSources) where

import Data.Either (fromRight)
import Data.Text qualified as T
import Data.Vector qualified as V (fromList)
import Statistics.Quantile (median, medianUnbiased)
import Servant.Client (BaseUrl, ClientEnv, mkClientEnv)
import Network.HTTP.Client.TLS (newTlsManager)

import Network.Coinbase.Client (coinbaseBaseUrl, getSpotPrice)
import Network.Kraken.Client (krakenBaseUrl, getOHLCData)
import PriceData.Types (CurrencyPair(..))

adaUsd :: CurrencyPair
adaUsd = CurrencyPair {
    _base = "ADA"
  , _currency = "USD"
  }

getCoinbasePrice :: (BaseUrl -> ClientEnv) -> IO (Either T.Text Double)
getCoinbasePrice env = do
  _ <- getSpotPrice (env coinbaseBaseUrl) adaUsd Nothing
  undefined

getKrakenPrice :: (BaseUrl -> ClientEnv) -> IO (Either T.Text Double)
getKrakenPrice env = do
  _ <- getOHLCData (env krakenBaseUrl) adaUsd Nothing Nothing
  undefined

-- see catMaybes

getMedianPriceFromSources :: IO Double
getMedianPriceFromSources = do
  manager <- newTlsManager
  let clientEnv = mkClientEnv manager
      fetchers :: [IO (Either T.Text Double)]
      fetchers = ($ clientEnv) <$> [getCoinbasePrice, getKrakenPrice]
  results <- (fmap $ fromRight 1) <$> sequenceA fetchers
  return $ median medianUnbiased $ V.fromList results

