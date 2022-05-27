module Main where

import Network.Connection (TLSSettings (..))
import Network.HTTP.Client.TLS (mkManagerSettings, tlsManagerSettings)
import Options.Applicative

import PriceFetcher (getMedianPriceFromSources)

main :: IO ()
main = do
  shouldDisableValidateCert <- execParser priceFeederOptions
  let dontValidateCertTlsSettings =
        TLSSettingsSimple
          { settingDisableCertificateValidation = True
          , settingDisableSession = False
          , settingUseServerName = False
          }
  putStrLn =<< show
    <$> getMedianPriceFromSources
      ( if shouldDisableValidateCert
          then mkManagerSettings dontValidateCertTlsSettings Nothing
          else tlsManagerSettings
      )

priceFeederOptions :: ParserInfo Bool
priceFeederOptions =
  info
    (disableCerticicateValidation <**> helper)
    ( fullDesc
        <> progDesc "Fetches the current Ada price from Binance, Coinbase, Huobi, Kraken and Kukoin and computes the median"
        <> header "ada-price-feeder"
    )

disableCerticicateValidation :: Parser Bool
disableCerticicateValidation = flag False True (long "disable-cert-validation")
