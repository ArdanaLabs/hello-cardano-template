{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Huobi.Server.Mock (huobiMockApp, mockGetTick) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (eitherDecodeFileStrict)
import Data.ByteString.Lazy.Char8 qualified as BSL
import Data.Maybe (fromJust)
import Data.Text qualified as T
import Network.Wai (Application)
import Servant (throwError)
import Servant.Server (Handler, err400, err500, errBody, serve)

import Network.Huobi.API (Tick (..), TickResponse (..), marketDataAPIProxy)
import PriceData (PriceData (..))

huobiMockApp :: FilePath -> Application
huobiMockApp priceDataPath = serve marketDataAPIProxy (mockGetTick priceDataPath)

mockGetTick :: FilePath -> Maybe T.Text -> Handler TickResponse
mockGetTick priceDataPath (Just "ADAUSDT") = do
  eitherPriceData <- liftIO $ eitherDecodeFileStrict priceDataPath
  either
    (\msg -> throwError $ err500 {errBody = BSL.pack msg})
    ( \price ->
        let (ask, bid) = fromJust price
         in return $
              TickResponse
                { _channel = "market.adausdt.detail.merged"
                , _status = "ok"
                , _timestamp = 1629788763750
                , _tick =
                    Tick
                      { _id = 272156789143
                      , _version = 272156789143
                      , _open = 50080.0
                      , _close = 49820.92
                      , _low = 48767.0
                      , _high = 50500.0
                      , _amount = 12055.365781937457
                      , _vol = 5.985618685709001E8
                      , _count = 420573
                      , _bid = (bid, 2.58112)
                      , _ask = (ask, 0.002411)
                      }
                }
    )
    (_huobiPrice <$> eitherPriceData)
mockGetTick _ _ = throwError $ err400 {errBody = "Invalid symbol."}
