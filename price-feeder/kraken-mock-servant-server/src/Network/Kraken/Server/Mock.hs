{-# LANGUAGE OverloadedStrings #-}
module Network.Kraken.Server.Mock (krakenMockApp, marketDataServer) where

import qualified Data.Map as Map
import Data.Text
import Data.Time.Clock.POSIX (POSIXTime)
import Network.Wai (Application)
import Servant ((:<|>)(..))
import Servant.Server (Server, Handler, serve)

import Network.Kraken.API 

krakenMockApp :: Application
krakenMockApp = serve marketDataAPIProxy marketDataServer

marketDataServer :: Server MarketDataAPI
marketDataServer = mockTickerInformationHandler :<|> mockOHLCHandler

mockTickerInformationHandler :: Maybe Text -> Handler (KrakenResponse AssetTickerInfoResponseUnsafe)
mockTickerInformationHandler Nothing =
  return $
    KrakenResponse {
      _error = [ "EGeneral:Invalid arguments:ordertype" ]
    , _result = AssetTickerInfoResponseUnsafe Map.empty
    }
mockTickerInformationHandler (Just ticker) =
  return $
    KrakenResponse {
      _error = []
    , _result = AssetTickerInfoResponseUnsafe $ Map.singleton ticker $
        AssetTickerInfoUnsafe
          ("52609.60000", "1", "1.00")
          ("52609.50000", "1", "1.00")
          ("52641.10000", "0.00080000")
          ("1920.83610601", "7954.00219674")
          ("52389.94668", "54022.90683")
          (23329, 80463)
          ("51513.90000", "51513.90000")
          ("51513.90000", "51513.90000")
          "52280.40000"
    }

mockOHLCHandler :: Maybe Text -> Maybe Integer -> Maybe POSIXTime -> Handler (KrakenResponse OHLCResponseUnsafe)
mockOHLCHandler Nothing _ _ =
  return $
    KrakenResponse {
      _error = [ "please provide a currency pair" ]
    , _result = OHLCResponseUnsafe 0 (Map.empty)
    }
mockOHLCHandler (Just currencyPair) _ _ =
  return $
    KrakenResponse {
      _error = []
    , _result = OHLCResponseUnsafe {
                  _ohlcResponseUnsafeLast = 1616662920
                , _ohlcResponseUnsafePairs = Map.fromList [
                   (currencyPair, [
                      (1616662740, "52591.9", "52599.9", "52591.8", "52599.9", "52599.1", "0.11091626", 5)
                    , (1616662980, "52601.2", "52601.2", "52599.9", "52599.9", "52599.9", "0.43748934", 7)
                    ])
                  ]
                 }
    }
  