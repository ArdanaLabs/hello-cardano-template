{-# LANGUAGE OverloadedStrings #-}

module Network.Binance.Server.Mock (binanceMockApp, mockTickerPriceHandler) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (eitherDecodeFileStrict)
import Data.ByteString.Lazy.Char8 qualified as BSL
import Data.Maybe (fromJust)
import Data.Text qualified as T
import Network.Wai (Application)
import Servant (throwError)
import Servant.Server (Handler, err400, err500, errBody, serve)

import Network.Binance.API (PriceResponse (..), tickerAPIProxy)
import PriceData (PriceData (..))

binanceMockApp :: FilePath -> Application
binanceMockApp priceDataPath = serve tickerAPIProxy (mockTickerPriceHandler priceDataPath)

mockTickerPriceHandler :: FilePath -> Maybe T.Text -> Handler PriceResponse
mockTickerPriceHandler priceDataPath (Just "ADAUSDT") = do
  eitherPriceData <- liftIO $ eitherDecodeFileStrict priceDataPath
  either
    (\msg -> throwError $ err500 {errBody = BSL.pack msg})
    (\price -> return $ PriceResponse {_symbol = "ADAUSDT", _price = T.pack $ show $ fromJust price})
    (_binancePrice <$> eitherPriceData)
mockTickerPriceHandler _ _ = throwError $ err400 {errBody = "Invalid symbol."}
