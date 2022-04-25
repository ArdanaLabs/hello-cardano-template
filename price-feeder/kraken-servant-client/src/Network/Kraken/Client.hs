{-# LANGUAGE OverloadedStrings #-}
module Network.Kraken.Client (krakenBaseUrl, getAssetTickerInformation, getOHLCData) where

import Servant.API
import Servant.Client
import Data.Text
import Data.Time.Clock.POSIX (POSIXTime)
import qualified Data.Map as Map 
import UnliftIO (MonadIO)
import UnliftIO.Exception (fromEitherIO, throwString)

import Network.Kraken.API
import Network.Kraken.Types (TickerData, toTickerData)

krakenBaseUrl :: BaseUrl
krakenBaseUrl = BaseUrl {
                  baseUrlScheme = Https
                , baseUrlHost = "api.kraken.com"
                , baseUrlPort = 443
                , baseUrlPath = "/0"
                }

getAssetTickerInformation :: MonadIO m => ClientEnv -> Text -> m AssetTickerInfoUnsafe
getAssetTickerInformation clientEnv ticker = do
  -- TODO use retry on connection clienterror
  let (tickerInformationClient :<|> _) = client marketDataAPIProxy
  krakenResponse <- fromEitherIO $ runClientM (tickerInformationClient (Just ticker)) clientEnv
  case krakenResponse of
    (KrakenResponse [] (AssetTickerInfoResponseUnsafe tickerMap)) -> do
      maybe (throwString $ "The fetched asset ticker information doesn't contain the expected ticker: " ++ (unpack ticker))
            return $
            Map.lookup ticker tickerMap
    (KrakenResponse errors _) -> throwString $ show errors

getOHLCData :: MonadIO m => ClientEnv -> Text -> Text -> Maybe Integer -> Maybe POSIXTime -> m [TickerData]
getOHLCData clientEnv base currency interval since = do
  let (_ :<|> ohlcDataClient) = client marketDataAPIProxy
      currencyPair = base <> currency
  krakenResponse <- fromEitherIO $ runClientM (ohlcDataClient (Just currencyPair) interval since) clientEnv
  case krakenResponse of
    (KrakenResponse [] (OHLCResponseUnsafe _last _pairs)) -> do
      _tickerData <- maybe (throwString $ "The fetched asset ticker information doesn't contain the expected ticker: " ++ (unpack currencyPair))
                      return $
                      Map.lookup currencyPair _pairs
      either throwString return (mapM toTickerData _tickerData)
    (KrakenResponse errors _) -> throwString $ show errors
      