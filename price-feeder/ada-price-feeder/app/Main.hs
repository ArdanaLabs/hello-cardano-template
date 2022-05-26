module Main where

import Network.Connection (TLSSettings (..))
import Network.HTTP.Client.TLS (mkManagerSettings, tlsManagerSettings)
import Options.Applicative

import PriceFetcher (getMedianPriceFromSources)

main :: IO ()
main = do
  shouldValidateCert <- execParser priceFeederOptions
  let dontValidateCertTlsSettings =
        TLSSettingsSimple
          { settingDisableCertificateValidation = False
          , settingDisableSession = False
          , settingUseServerName = False
          }
  putStrLn =<< show
    <$> getMedianPriceFromSources
      ( if shouldValidateCert
          then tlsManagerSettings
          else mkManagerSettings dontValidateCertTlsSettings Nothing
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
