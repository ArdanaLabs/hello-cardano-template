{-# LANGUAGE OverloadedStrings #-}
module Network.Binance.Server.Mock (binanceMockApp, mockTickerPriceHandler) where

import qualified Data.Text as T
import Network.Wai (Application)
import Servant (throwError)
import Servant.Server (Handler, serve, errBody, err400)

import Network.Binance.API (PriceResponse(..), tickerAPIProxy)

binanceMockApp :: Double -> Application
binanceMockApp price = serve tickerAPIProxy (mockTickerPriceHandler price)

mockTickerPriceHandler :: Double -> Maybe T.Text -> Handler PriceResponse
mockTickerPriceHandler price (Just "ADAUSDT") = return $ PriceResponse { _symbol = "ADAUSDT", _price = T.pack $ show price }
mockTickerPriceHandler _ _ = throwError $ err400 { errBody = "Invalid symbol." }
