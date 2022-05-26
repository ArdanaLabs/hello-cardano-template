{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module PriceFetcher (getMedianPriceFromSources) where

import Control.Monad (void)
import Data.Either (partitionEithers)
import Data.Vector qualified as V (fromList)
import Network.HTTP.Client (ManagerSettings)
import Network.HTTP.Client.TLS (newTlsManagerWith)
import Servant.Client (BaseUrl, ClientEnv, mkClientEnv)
import Statistics.Quantile (median, medianUnbiased)
import System.IO (hPutStrLn, stderr)
import UnliftIO.Exception (throwString, tryAny)

import Clients

fetchers :: (BaseUrl -> ClientEnv) -> [IO Double]
fetchers mkEnv =
  [ getBinancePrice (mkEnv binanceBaseUrl)
  , getCoinbasePrice (mkEnv coinbaseBaseUrl)
  , getHuobiPrice (mkEnv huobiBaseUrl)
  , getKrakenPrice (mkEnv krakenBaseUrl)
  , getKucoinPrice (mkEnv kucoinBaseUrl)
  ]

getMedianPriceFromSources :: ManagerSettings -> IO Double
getMedianPriceFromSources managerSettings = do
  manager <- newTlsManagerWith managerSettings
  let clientEnv = mkClientEnv manager
  (errors, prices) <- partitionEithers <$> traverse tryAny (fetchers clientEnv)
  void $ traverse (hPutStrLn stderr . show) errors
  if length prices > 0
    then return $ median medianUnbiased $ V.fromList prices
    else do
      throwString "Unable to fetch at least one price"
