module Main where

import Network.Wai.Handler.Warp (defaultSettings, setPort)
import Network.Wai.Handler.WarpTLS as Warp (defaultTlsSettings, runTLS)
import Options.Applicative

import CLI (Serve (..), mockServerOptions)
import Network.Coinbase.Server.Mock (coinbaseMockApp)

main :: IO ()
main = do
  execParser (mockServerOptions "coinbase") >>= \(Serve priceDataPath port) ->
    runTLS defaultTlsSettings (setPort port defaultSettings) (coinbaseMockApp priceDataPath)
