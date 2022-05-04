{-# LANGUAGE OverloadedStrings #-}
module Network.Huobi.Client (huobiBaseUrl, getTick) where

import Servant.Client
import UnliftIO (MonadIO)
import UnliftIO.Exception (fromEitherIO)

import Network.Huobi.API (TickResponse(..), Tick(..), marketDataAPIProxy)
import PriceData.Types (CurrencyPair(..))

huobiBaseUrl :: BaseUrl
huobiBaseUrl = BaseUrl {
                   baseUrlScheme = Https
                 , baseUrlHost = "api.huobi.pro"
                 , baseUrlPort = 443
                 , baseUrlPath = ""
                 }

getTick:: MonadIO m => ClientEnv -> CurrencyPair -> m Tick
getTick clientEnv (CurrencyPair base currency) = do
  -- TODO use retry on connection clienterror
  _tick <$> (fromEitherIO $ runClientM (client marketDataAPIProxy (Just $ base <> currency)) clientEnv)
