{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.Kucoin.Server.Mock (kucoinMockApp, mockFiatPriceHandler) where

import Data.Map qualified as M
import Data.Text qualified as T
import Network.Wai (Application)
import Servant (throwError)
import Servant.Server (Handler, serve, errBody, err400)

import Network.Kucoin.API (FiatPriceResponse(..), fiatPriceAPIProxy)

kucoinMockApp :: Double -> Application
kucoinMockApp price = serve fiatPriceAPIProxy (mockFiatPriceHandler price)

mockFiatPriceHandler :: Double -> Maybe T.Text -> [T.Text] -> Handler FiatPriceResponse
mockFiatPriceHandler price (Just "USD") ["ADA"] = return $ FiatPriceResponse { _code = "200000", _data = M.singleton "ADA" $ T.pack $ show price }
mockFiatPriceHandler _ _ _ = throwError $ err400 { errBody = "Not Found." }
