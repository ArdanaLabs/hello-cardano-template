{-# LANGUAGE OverloadedStrings #-}
module Network.Coinbase.Client (coinbaseBaseUrl, getSpotPrice) where

import Servant.Client
import Data.Text
import Data.Time (Day)
import UnliftIO (MonadIO)
import UnliftIO.Exception (fromEitherIO, throwString)

import Network.Coinbase.API (CoinbaseResponse(..), SpotPriceUnsafe, pricesAPIProxy)

coinbaseBaseUrl :: BaseUrl
coinbaseBaseUrl = BaseUrl {
                    baseUrlScheme = Https
                  , baseUrlHost = "api.coinbase.com"
                  , baseUrlPort = 443
                  , baseUrlPath = "/v2"
                  }

getSpotPrice :: MonadIO m => ClientEnv -> Text -> Text -> Maybe Day -> m SpotPriceUnsafe
getSpotPrice clientEnv base currency maybeDay = do
  -- TODO use retry on connection clienterror
  coinbaseResponse <- fromEitherIO $ runClientM (client pricesAPIProxy (base <> "-" <> currency) ((pack . show) <$> maybeDay)) clientEnv
  case coinbaseResponse of
    (CoinbaseResponse (Just errors) _) -> throwString $ show errors
    (CoinbaseResponse Nothing (Just spotPrice)) -> return spotPrice
    (CoinbaseResponse Nothing Nothing) -> throwString "Fatal error :O"

