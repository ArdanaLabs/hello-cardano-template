module Main where

import Network.Wai.Handler.Warp (setPort, defaultSettings)
import Network.Wai.Handler.WarpTLS as Warp (runTLS, defaultTlsSettings)
import Options.Applicative

import Network.Binance.Server.Mock (binanceMockApp)
import CLI (Serve(..), mockServerOptions)

main :: IO ()
main = do
  execParser (mockServerOptions "kraken") >>= \(Serve priceDataPath port) ->
    runTLS defaultTlsSettings (setPort port defaultSettings) (binanceMockApp priceDataPath)
