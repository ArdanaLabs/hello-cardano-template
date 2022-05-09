module Main where

import Network.Wai.Handler.Warp (setPort, defaultSettings)
import Network.Wai.Handler.WarpTLS as Warp (runTLS, defaultTlsSettings)
import Options.Applicative

import Network.Coinbase.Server.Mock (coinbaseMockApp)
import CLI (Serve(..), mockServerOptions)

main :: IO ()
main = do
  execParser (mockServerOptions "coinbase") >>= \(Serve priceDataPath port) ->
    runTLS defaultTlsSettings (setPort port defaultSettings) (coinbaseMockApp priceDataPath)
