{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module PriceFetcher (getMedianPriceFromSources) where

import Data.Either (partitionEithers)
import Data.Foldable (traverse_)
import Data.Vector qualified as V (fromList)
import Network.HTTP.Client (ManagerSettings)
import Network.HTTP.Client.TLS (newTlsManagerWith)
import Servant.Client (BaseUrl, ClientEnv, mkClientEnv)
import Statistics.Quantile (median, medianUnbiased)
import System.IO (hPrint, stderr)
import UnliftIO.Exception (throwString, tryAny)
import UnliftIO.Internals.Async (mapConcurrently)

import Clients

fetchers :: (BaseUrl -> ClientEnv) -> [IO Double]
fetchers mkEnv =
  [ getBinancePrice (mkEnv binanceBaseUrl)
  , getCoinbasePrice (mkEnv coinbaseBaseUrl)
  , getHuobiPrice (mkEnv huobiBaseUrl)
  , getKrakenPrice (mkEnv krakenBaseUrl)
  , getKucoinPrice (mkEnv kucoinBaseUrl)
  ]

getMedianPriceFromSources :: Int -> ManagerSettings -> IO Double
getMedianPriceFromSources minNumberOfPrices managerSettings = do
  manager <- newTlsManagerWith managerSettings
  let clientEnv = mkClientEnv manager
  (errors, prices) <- partitionEithers <$> mapConcurrently tryAny (fetchers clientEnv)
  traverse_ (hPrint stderr) errors
  if length prices >= minNumberOfPrices
    then return $ median medianUnbiased $ V.fromList prices
    else do
      throwString "Unable to fetch the minimal required number of prices"
