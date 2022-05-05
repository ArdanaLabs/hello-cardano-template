{-# LANGUAGE OverloadedStrings #-}
module Network.Kucoin.Client (kucoinBaseUrl, getFiatPrice) where

import Servant.Client
import UnliftIO (MonadIO)
import UnliftIO.Exception (fromEitherIO)

import Network.Kucoin.API (FiatPriceResponse(..), fiatPriceAPIProxy)
import PriceData.Types (CurrencyPair(..))

kucoinBaseUrl :: BaseUrl
kucoinBaseUrl = BaseUrl {
                  baseUrlScheme = Https
                , baseUrlHost = "api.kucoin.com"
                , baseUrlPort = 443
                , baseUrlPath = "/api"
                }

getFiatPrice :: MonadIO m => ClientEnv -> CurrencyPair -> m FiatPriceResponse
getFiatPrice clientEnv (CurrencyPair base currency) = do
  -- TODO use retry on connection clienterror
  fromEitherIO $ runClientM (client fiatPriceAPIProxy (Just currency) [base]) clientEnv
