{-# LANGUAGE OverloadedStrings #-}
module Network.Huobi.Server.Mock (huobiMockApp, mockGetTick) where

import qualified Data.Text as T
import Network.Wai (Application)
import Servant (throwError)
import Servant.Server (Handler, serve, errBody, err400)

import Network.Huobi.API (TickResponse(..), Tick(..), marketDataAPIProxy)

huobiMockApp :: Application
huobiMockApp = serve marketDataAPIProxy mockGetTick

mockGetTick :: Maybe T.Text -> Handler TickResponse
mockGetTick (Just "ADAUSDT") = return $ TickResponse {
                                          _channel = "market.adausdt.detail.merged"
                                        , _status = "ok"
                                        , _timestamp = 1629788763750
                                        , _tick = Tick {
                                                    _id = 272156789143
                                                  , _version = 272156789143
                                                  , _open = 50080.0
                                                  , _close = 49820.92
                                                  , _low = 48767.0
                                                  , _high = 50500.0
                                                  , _amount = 12055.365781937457
                                                  , _vol = 5.985618685709001E8
                                                  , _count = 420573
                                                  , _bid = (49819.48, 2.58112)
                                                  , _ask = (49819.49, 0.002411)
                                                  }
                                        }
mockGetTick _ = throwError $ err400 { errBody = "Invalid symbol." }
