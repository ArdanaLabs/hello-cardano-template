{-# LANGUAGE OverloadedStrings #-}
module Network.Kraken.Client (krakenBaseUrl, getAssetTickerInformation) where

import Servant.Client
-- import Control.Monad.Reader (ReaderT, ask)
import Data.Text
import qualified Data.Map as Map 
import UnliftIO (MonadIO)
import UnliftIO.Exception (fromEitherIO, throwString)

import Network.Kraken.API (KrakenResponse(..), AssetTickerInfoUnsafe, marketDataAPIProxy)

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
  krakenResponse <- fromEitherIO $ runClientM (client marketDataAPIProxy (Just ticker)) clientEnv
  case krakenResponse of
    (KrakenResponse [] tickerMap) -> do
      maybe (throwString $ "The fetched asset ticker information doesn't contain the expected ticker: " ++ (unpack ticker))
            return $
            Map.lookup ticker tickerMap
    (KrakenResponse errors _) -> throwString $ show errors

