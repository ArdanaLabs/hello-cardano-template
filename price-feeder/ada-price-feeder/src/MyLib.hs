{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}
module MyLib (getMedianPriceFromSources) where

import Data.Vector qualified as V (fromList)
import Statistics.Quantile (median, medianUnbiased)
import Servant.Client (BaseUrl, ClientEnv, mkClientEnv)
import Network.HTTP.Client.TLS (newTlsManager)

import Clients

fetchers :: (BaseUrl -> ClientEnv) -> [IO Double]
fetchers mkEnv = [ getBinancePrice (mkEnv binanceBaseUrl)
                 , getCoinbasePrice (mkEnv coinbaseBaseUrl)
                 , getHuobiPrice (mkEnv huobiBaseUrl)
                 , getKrakenPrice (mkEnv krakenBaseUrl)
                 , getKucoinPrice (mkEnv kucoinBaseUrl)
                 ]

getMedianPriceFromSources :: IO Double
getMedianPriceFromSources = do
  manager <- newTlsManager
  let clientEnv = mkClientEnv manager
  results <-sequenceA $ fetchers clientEnv
  return $ median medianUnbiased $ V.fromList results

