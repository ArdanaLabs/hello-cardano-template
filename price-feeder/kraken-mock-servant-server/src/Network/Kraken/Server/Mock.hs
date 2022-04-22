{-# LANGUAGE OverloadedStrings #-}
module Network.Kraken.Server.Mock (krakenMockApp, mockTickerInformationHandler) where

import Data.Text
import qualified Data.Map as Map
import Network.Wai (Application)
import Servant.Server (Handler, serve)

import Network.Kraken.API (KrakenResponse(..), AssetTickerInfoUnsafe(..), marketDataAPIProxy)

krakenMockApp :: Application
krakenMockApp = serve marketDataAPIProxy mockTickerInformationHandler

mockTickerInformationHandler :: Maybe Text -> Handler (KrakenResponse AssetTickerInfoUnsafe)
mockTickerInformationHandler Nothing =
  return $
    KrakenResponse {
      _error = [ "EGeneral:Invalid arguments:ordertype" ]
    , _result = Map.empty
    }
mockTickerInformationHandler (Just ticker) =
  return $
    KrakenResponse {
      _error = []
    , _result = Map.singleton ticker $
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
    
  