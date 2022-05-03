{-# LANGUAGE OverloadedStrings #-}
module Network.Binance.Client (binanceBaseUrl, getPrice) where

import Servant.Client
import UnliftIO (MonadIO)
import UnliftIO.Exception (fromEitherIO)

import Network.Binance.API (PriceUnsafe, tickerAPIProxy)
import PriceData.Types (CurrencyPair(..))

binanceBaseUrl :: BaseUrl
binanceBaseUrl = BaseUrl {
                   baseUrlScheme = Https
                 , baseUrlHost = "api.binance.com"
                 , baseUrlPort = 443
                 , baseUrlPath = "/api/v3"
                 }

getPrice :: MonadIO m => ClientEnv -> CurrencyPair -> m PriceUnsafe
getPrice clientEnv (CurrencyPair base currency) = do
  -- TODO use retry on connection clienterror
  fromEitherIO $ runClientM (client tickerAPIProxy (Just $ base <> currency)) clientEnv
