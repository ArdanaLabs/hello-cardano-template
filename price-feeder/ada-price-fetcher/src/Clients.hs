{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Clients where

import Control.Retry (RetryPolicyM, RetryStatus, exponentialBackoff, limitRetries, retrying)
import Data.Map qualified as M
import Data.Text qualified as T
import Data.Text.Read (double)
import Servant.API ((:<|>) (..))
import Servant.Client (BaseUrl (..), ClientEnv, ClientError (..), Scheme (..), client, runClientM)
import UnliftIO (MonadIO)
import UnliftIO.Exception (fromEitherIO, stringException, throwIO, throwString)

import Network.Binance.API (PriceResponse (..), tickerAPIProxy)
import Network.Coinbase.API (CoinbaseResponse (..), SpotPrice (SpotPrice), pricesAPIProxy)
import Network.Huobi.API (Tick (..), TickResponse (..), marketDataAPIProxy)
import Network.Kraken.API (AssetTickerInfoResponse (AssetTickerInfoResponse), KrakenResponse (..), _volumeWeightedAveragePrice)
import Network.Kraken.API qualified as Kraken
import Network.Kucoin.API (FiatPriceResponse (..), fiatPriceAPIProxy)

import Types (CurrencyPair (..))

fromEither' :: MonadIO m => Either String a -> m a
fromEither' (Left e) = throwIO . stringException $ e
fromEither' (Right x) = pure x

eitherDouble :: T.Text -> Either String Double
eitherDouble text = do
  res <- double text
  case res of
    (x, "") -> Right x
    (_, remainder) -> Left $ "Unable to read " <> T.unpack remainder <> " from input " <> T.unpack text

adaUsd :: CurrencyPair
adaUsd =
  CurrencyPair
    { _base = "ADA"
    , _currency = "USD"
    }

adaUsdt :: CurrencyPair
adaUsdt =
  CurrencyPair
    { _base = "ADA"
    , _currency = "USDT"
    }

defaultRetryPolicy :: Monad m => RetryPolicyM m
defaultRetryPolicy = exponentialBackoff 2000000 <> limitRetries 1

-- Retry on connection error
retryDecider :: Monad m => RetryStatus -> Either ClientError a -> m Bool
retryDecider _ (Left (ConnectionError _)) = pure True
retryDecider _ _ = pure False

binanceBaseUrl :: BaseUrl
binanceBaseUrl =
  BaseUrl
    { baseUrlScheme = Https
    , baseUrlHost = "api.binance.com"
    , baseUrlPort = 443
    , baseUrlPath = "/api/v3"
    }

getBinancePrice :: ClientEnv -> IO Double
getBinancePrice clientEnv = do
  let CurrencyPair base currency = adaUsdt
  (PriceResponse _ priceText) <- fromEitherIO $ retrying defaultRetryPolicy retryDecider $ \_ -> runClientM (client tickerAPIProxy (Just $ base <> currency)) clientEnv
  fromEither' $ eitherDouble priceText

coinbaseBaseUrl :: BaseUrl
coinbaseBaseUrl =
  BaseUrl
    { baseUrlScheme = Https
    , baseUrlHost = "api.coinbase.com"
    , baseUrlPort = 443
    , baseUrlPath = "/v2"
    }

getCoinbasePrice :: ClientEnv -> IO Double
getCoinbasePrice clientEnv = do
  let CurrencyPair base currency = adaUsd
  coinbaseResponse <- fromEitherIO $ retrying defaultRetryPolicy retryDecider $ \_ -> runClientM (client pricesAPIProxy (base <> "-" <> currency) Nothing) clientEnv
  case coinbaseResponse of
    (CoinbaseResponse (Just errors) _) -> throwString $ show errors
    (CoinbaseResponse Nothing (Just (SpotPrice priceText _ _))) -> fromEither' $ eitherDouble priceText
    (CoinbaseResponse Nothing Nothing) -> throwString "Fatal error :O"

kucoinBaseUrl :: BaseUrl
kucoinBaseUrl =
  BaseUrl
    { baseUrlScheme = Https
    , baseUrlHost = "api.kucoin.com"
    , baseUrlPort = 443
    , baseUrlPath = "/api"
    }

getKucoinPrice :: MonadIO m => ClientEnv -> m Double
getKucoinPrice clientEnv = do
  let CurrencyPair base currency = adaUsd
  (FiatPriceResponse _ dataMap) <- fromEitherIO $ retrying defaultRetryPolicy retryDecider $ \_ -> runClientM (client fiatPriceAPIProxy (Just currency) [base]) clientEnv
  let eitherPriceText = maybe (Left $ "Couldn't find base symbol in the received response: " <> T.unpack base) Right $ M.lookup base dataMap
  fromEither' $ eitherDouble =<< eitherPriceText

huobiBaseUrl :: BaseUrl
huobiBaseUrl =
  BaseUrl
    { baseUrlScheme = Https
    , baseUrlHost = "api.huobi.pro"
    , baseUrlPort = 443
    , baseUrlPath = ""
    }

getHuobiPrice :: MonadIO m => ClientEnv -> m Double
getHuobiPrice clientEnv = do
  let CurrencyPair base currency = adaUsdt
      symbolParam = T.toLower $ base <> currency
  tick <- _tick <$> fromEitherIO (retrying defaultRetryPolicy retryDecider $ \_ -> runClientM (client marketDataAPIProxy (Just symbolParam)) clientEnv)
  let (ask, _) = _ask tick
      (bid, _) = _bid tick
  pure $ (ask + bid) / 2

krakenBaseUrl :: BaseUrl
krakenBaseUrl =
  BaseUrl
    { baseUrlScheme = Https
    , baseUrlHost = "api.kraken.com"
    , baseUrlPort = 443
    , baseUrlPath = "/0"
    }

getKrakenPrice :: ClientEnv -> IO Double
getKrakenPrice clientEnv = do
  let (tickerInformationClient :<|> _) = client Kraken.marketDataAPIProxy
      CurrencyPair base currency = adaUsd
      currencyPair = base <> currency
  krakenResponse <- fromEitherIO $ retrying defaultRetryPolicy retryDecider $ \_ -> runClientM (tickerInformationClient (Just currencyPair)) clientEnv
  case krakenResponse of
    (KrakenResponse [] (AssetTickerInfoResponse tickerMap)) -> do
      let eitherPriceText =
            maybe
              (Left $ "The fetched asset ticker information doesn't contain the expected ticker: " <> T.unpack currencyPair)
              (Right . fst . _volumeWeightedAveragePrice)
              $ M.lookup currencyPair tickerMap

      fromEither' $ eitherDouble =<< eitherPriceText
    (KrakenResponse errors _) -> throwString $ show errors
