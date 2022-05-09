module Main where

import Network.Wai.Handler.Warp (defaultSettings, setPort)
import Network.Wai.Handler.WarpTLS as Warp (defaultTlsSettings, runTLS)
import Options.Applicative

import CLI (Serve (..), mockServerOptions)
import Network.Kucoin.Server.Mock (kucoinMockApp)

main :: IO ()
main = do
  execParser (mockServerOptions "kucoin") >>= \(Serve priceDataPath port) ->
    runTLS defaultTlsSettings (setPort port defaultSettings) (kucoinMockApp priceDataPath)
