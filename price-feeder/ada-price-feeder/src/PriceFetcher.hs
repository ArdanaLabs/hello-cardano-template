{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module PriceFetcher (getMedianPriceFromSources) where

import Data.Either (rights)
import Data.Vector qualified as V (fromList)
import Network.HTTP.Client.TLS (newTlsManager)
import Servant.Client (BaseUrl, ClientEnv, mkClientEnv)
import Statistics.Quantile (median, medianUnbiased)
import UnliftIO.Exception (tryAny)

import Clients

fetchers :: (BaseUrl -> ClientEnv) -> [IO Double]
fetchers mkEnv =
  [ getBinancePrice (mkEnv binanceBaseUrl)
  , getCoinbasePrice (mkEnv coinbaseBaseUrl)
  , getHuobiPrice (mkEnv huobiBaseUrl)
  , getKrakenPrice (mkEnv krakenBaseUrl)
  , getKucoinPrice (mkEnv kucoinBaseUrl)
  ]

getMedianPriceFromSources :: IO Double
getMedianPriceFromSources = do
  manager <- newTlsManager
  let clientEnv = mkClientEnv manager
  results <- traverse tryAny (fetchers clientEnv)
  return $ median medianUnbiased $ V.fromList (rights results)
