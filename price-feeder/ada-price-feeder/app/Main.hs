module Main where

import Network.Connection (TLSSettings (..))
import Network.HTTP.Client.TLS (mkManagerSettings, tlsManagerSettings)
import Options.Applicative

import PriceFetcher (getMedianPriceFromSources)

main :: IO ()
main = do
  options <- execParser priceFeederOptions
  let dontValidateCertTlsSettings =
        TLSSettingsSimple
          { settingDisableCertificateValidation = True
          , settingDisableSession = False
          , settingUseServerName = False
          }
      managerSettings = if _disableCerticicateValidation options
                          then mkManagerSettings dontValidateCertTlsSettings Nothing
                          else tlsManagerSettings
  print =<< getMedianPriceFromSources (_minNumberOfPrices options) managerSettings

data PriceFeederOptions = PriceFeederOptions {
  _disableCerticicateValidation :: Bool
, _minNumberOfPrices :: Int
}

priceFeederOptions :: ParserInfo PriceFeederOptions
priceFeederOptions =
  info
    ((PriceFeederOptions <$> disableCerticicateValidation <*> minNumberOfPrices) <**> helper)
    ( fullDesc
        <> progDesc "Fetches the current Ada price from Binance, Coinbase, Huobi, Kraken and Kukoin and computes the median"
        <> header "ada-price-feeder"
    )

disableCerticicateValidation :: Parser Bool
disableCerticicateValidation = flag False True (long "disable-cert-validation")

minNumberOfPrices :: Parser Int
minNumberOfPrices = option auto
  ( long "min-prices"
  <> short 'N'
  <> metavar "INT"
  <> help "The minimal number of prices that needs to be fetched succesfully for the program to succeed"
  <> value 5
  <> showDefault )
