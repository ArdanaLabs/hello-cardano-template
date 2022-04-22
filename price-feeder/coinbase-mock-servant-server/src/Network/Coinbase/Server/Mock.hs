{-# LANGUAGE OverloadedStrings #-}
module Network.Coinbase.Server.Mock (coinbaseMockApp, mockSpotPriceHandler) where

import qualified Data.Text as T
import Data.Time (Day)
import Network.Wai (Application)
import Servant.Server (Handler, serve)
import UnliftIO.Exception (pureTry)

import Network.Coinbase.API (CoinbaseResponse(..), CoinbaseError(..), SpotPriceUnsafe(..), pricesAPIProxy)

coinbaseMockApp :: Application
coinbaseMockApp = serve pricesAPIProxy mockSpotPriceHandler

mockSpotPriceHandler :: T.Text -> Maybe T.Text -> Handler (CoinbaseResponse SpotPriceUnsafe)
mockSpotPriceHandler currencyPair maybeDay = do
  let (base, currency) = T.drop 1 <$> T.break (== '-') currencyPair
      successResponse = CoinbaseResponse Nothing $ Just $
                          SpotPriceUnsafe { _base = base, _currency = currency, _amount = "1232.44" }
  case maybeDay of
    Nothing -> return successResponse
    Just dateText -> do
      either (\ err -> return $ CoinbaseResponse (Just [ CoinbaseError { _id = "date error" , _message = (T.pack . show) err }]) Nothing)
             (\_ -> return successResponse) $
             pureTry (read $ T.unpack dateText :: Day)

  