{-# LANGUAGE OverloadedStrings #-}
module Network.Binance.Server.Mock (binanceMockApp, mockTickerPriceHandler) where

import qualified Data.Text as T
import Network.Wai (Application)
import Servant (throwError)
import Servant.Server (Handler, serve, errBody, err400)

import Network.Binance.API (PriceUnsafe(..), tickerAPIProxy)

binanceMockApp :: Application
binanceMockApp = serve tickerAPIProxy mockTickerPriceHandler

mockTickerPriceHandler :: Maybe T.Text -> Handler PriceUnsafe
mockTickerPriceHandler (Just "ADAUSDT") = return $ PriceUnsafe { _symbol = "ADAUSDT", _price = "0.79260000" }
mockTickerPriceHandler _ = throwError $ err400 { errBody = "Invalid symbol." }
