module Main where

import Network.Wai.Handler.Warp (defaultSettings, setPort)
import Network.Wai.Handler.WarpTLS as Warp (defaultTlsSettings, runTLS)
import Options.Applicative

import CLI (Serve (..), mockServerOptions)
import Network.Huobi.Server.Mock (huobiMockApp)

main :: IO ()
main = do
  execParser (mockServerOptions "huobi") >>= \(Serve priceDataPath port) ->
    runTLS defaultTlsSettings (setPort port defaultSettings) (huobiMockApp priceDataPath)
