{-# LANGUAGE OverloadedStrings #-}
module Network.Kraken.Server.Mock (krakenMockApp, marketDataServer) where

import qualified Data.Map as Map
import Data.Text
import Data.Time.Clock.POSIX (POSIXTime)
import Network.Wai (Application)
import Servant ((:<|>)(..))
import Servant.Server (Server, Handler, serve)

import Network.Kraken.API 

krakenMockApp :: Double -> Application
krakenMockApp price = serve marketDataAPIProxy (marketDataServer price)

marketDataServer :: Double -> Server MarketDataAPI
marketDataServer price = (mockTickerInformationHandler price) :<|> mockOHLCHandler

mockTickerInformationHandler :: Double -> Maybe Text -> Handler (KrakenResponse AssetTickerInfoResponse)
mockTickerInformationHandler _ Nothing =
  return $
    KrakenResponse {
      _error = [ "EGeneral:Invalid arguments:ordertype" ]
    , _result = AssetTickerInfoResponse Map.empty
    }
mockTickerInformationHandler price (Just ticker) =
  return $
    KrakenResponse {
      _error = []
    , _result = AssetTickerInfoResponse $ Map.singleton ticker $
        AssetTickerInfo
          ("52609.60000", "1", "1.00")
          ("52609.50000", "1", "1.00")
          ("52641.10000", "0.00080000")
          ("1920.83610601", "7954.00219674")
          (pack $ show price, "54022.90683")
          (23329, 80463)
          ("51513.90000", "51513.90000")
          ("51513.90000", "51513.90000")
          "52280.40000"
    }

mockOHLCHandler :: Maybe Text -> Maybe Integer -> Maybe POSIXTime -> Handler (KrakenResponse OHLCResponse)
mockOHLCHandler Nothing _ _ =
  return $
    KrakenResponse {
      _error = [ "please provide a currency pair" ]
    , _result = OHLCResponse 0 (Map.empty)
    }
mockOHLCHandler (Just currencyPair) _ _ =
  return $
    KrakenResponse {
      _error = []
    , _result = OHLCResponse {
                  _ohlcResponseLast = 1616662920
                , _ohlcResponsePairs = Map.fromList [
                   (currencyPair, [
                      (1616662740, "52591.9", "52599.9", "52591.8", "52599.9", "52599.1", "0.11091626", 5)
                    , (1616662980, "52601.2", "52601.2", "52599.9", "52599.9", "52599.9", "0.43748934", 7)
                    ])
                  ]
                 }
    }
  