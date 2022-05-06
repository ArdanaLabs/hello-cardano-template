{-# LANGUAGE OverloadedStrings #-}
module Network.Coinbase.Server.Mock (coinbaseMockApp, mockSpotPriceHandler) where

import qualified Data.Text as T
import Data.Time (Day)
import Network.Wai (Application)
import Servant.Server (Handler, serve)
import UnliftIO.Exception (pureTry)

import Network.Coinbase.API (CoinbaseResponse(..), CoinbaseError(..), SpotPrice(..), pricesAPIProxy)

coinbaseMockApp :: Double -> Application
coinbaseMockApp price = serve pricesAPIProxy (mockSpotPriceHandler price)

mockSpotPriceHandler :: Double -> T.Text -> Maybe T.Text -> Handler (CoinbaseResponse SpotPrice)
mockSpotPriceHandler price currencyPair maybeDay = do
  let (base, currency) = T.drop 1 <$> T.break (== '-') currencyPair
      successResponse = CoinbaseResponse Nothing $ Just $
                          SpotPrice{ _base = base, _currency = currency, _amount = T.pack $ show price }
  case maybeDay of
    Nothing -> return successResponse
    Just dateText -> do
      either (\ err -> return $ CoinbaseResponse (Just [ CoinbaseError { _id = "date error" , _message = (T.pack . show) err }]) Nothing)
             (\_ -> return successResponse) $
             pureTry (read $ T.unpack dateText :: Day)

  