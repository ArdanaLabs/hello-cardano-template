{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Kraken.Server.Mock (krakenMockApp, marketDataServer) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (eitherDecodeFileStrict)
import Data.ByteString.Lazy.Char8 qualified as BSL
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.Text
import Data.Time.Clock.POSIX (POSIXTime)
import Network.Wai (Application)
import Servant (throwError, (:<|>) (..))
import Servant.Server (Handler, Server, err500, errBody, serve)

import Network.Kraken.API
import PriceData (PriceData (..))

krakenMockApp :: FilePath -> Application
krakenMockApp priceDataPath = serve marketDataAPIProxy (marketDataServer priceDataPath)

marketDataServer :: FilePath -> Server MarketDataAPI
marketDataServer priceDataPath = (mockTickerInformationHandler priceDataPath) :<|> mockOHLCHandler

mockTickerInformationHandler :: FilePath -> Maybe Text -> Handler (KrakenResponse AssetTickerInfoResponse)
mockTickerInformationHandler _ Nothing =
  return $
    KrakenResponse
      { _error = ["EGeneral:Invalid arguments:ordertype"]
      , _result = AssetTickerInfoResponse Map.empty
      }
mockTickerInformationHandler priceDataPath (Just ticker) = do
  eitherPriceData <- liftIO $ eitherDecodeFileStrict priceDataPath
  either
    (\msg -> throwError $ err500 {errBody = BSL.pack msg})
    ( \price ->
        return $
          KrakenResponse
            { _error = []
            , _result =
                AssetTickerInfoResponse $
                  Map.singleton ticker $
                    AssetTickerInfo
                      ("52609.60000", "1", "1.00")
                      ("52609.50000", "1", "1.00")
                      ("52641.10000", "0.00080000")
                      ("1920.83610601", "7954.00219674")
                      (pack $ show $ fromJust price, "54022.90683")
                      (23329, 80463)
                      ("51513.90000", "51513.90000")
                      ("51513.90000", "51513.90000")
                      "52280.40000"
            }
    )
    (_krakenPrice <$> eitherPriceData)

mockOHLCHandler :: Maybe Text -> Maybe Integer -> Maybe POSIXTime -> Handler (KrakenResponse OHLCResponse)
mockOHLCHandler Nothing _ _ =
  return $
    KrakenResponse
      { _error = ["please provide a currency pair"]
      , _result = OHLCResponse 0 (Map.empty)
      }
mockOHLCHandler (Just currencyPair) _ _ =
  return $
    KrakenResponse
      { _error = []
      , _result =
          OHLCResponse
            { _ohlcResponseLast = 1616662920
            , _ohlcResponsePairs =
                Map.fromList
                  [
                    ( currencyPair
                    ,
                      [ (1616662740, "52591.9", "52599.9", "52591.8", "52599.9", "52599.1", "0.11091626", 5)
                      , (1616662980, "52601.2", "52601.2", "52599.9", "52599.9", "52599.9", "0.43748934", 7)
                      ]
                    )
                  ]
            }
      }
