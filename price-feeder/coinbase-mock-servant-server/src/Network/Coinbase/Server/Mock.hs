{-# LANGUAGE OverloadedStrings #-}

module Network.Coinbase.Server.Mock (coinbaseMockApp, mockSpotPriceHandler) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (eitherDecodeFileStrict)
import Data.ByteString.Lazy.Char8 qualified as BSL
import Data.Maybe (fromJust)
import Data.Text qualified as T
import Network.Wai (Application)
import Servant (throwError)
import Servant.Server (Handler, err500, errBody, serve)

import Network.Coinbase.API (CoinbaseError (..), CoinbaseResponse (..), SpotPrice (..), pricesAPIProxy)
import PriceData (PriceData (..))

coinbaseMockApp :: FilePath -> Application
coinbaseMockApp priceDataPath = serve pricesAPIProxy (mockSpotPriceHandler priceDataPath)

mockSpotPriceHandler :: FilePath -> T.Text -> Maybe T.Text -> Handler (CoinbaseResponse SpotPrice)
mockSpotPriceHandler priceDataPath currencyPair Nothing = do
  let (base, currency) = T.drop 1 <$> T.break (== '-') currencyPair
  eitherPriceData <- liftIO $ eitherDecodeFileStrict priceDataPath
  either
    (\msg -> throwError $ err500 {errBody = BSL.pack msg})
    (\price -> return $ CoinbaseResponse Nothing $ Just $ SpotPrice {_base = base, _currency = currency, _amount = T.pack $ show $ fromJust price})
    (_coinbasePrice <$> eitherPriceData)
mockSpotPriceHandler _ _ _ = return $ CoinbaseResponse (Just [CoinbaseError {_id = "date error", _message = "date handler not implemented"}]) Nothing
