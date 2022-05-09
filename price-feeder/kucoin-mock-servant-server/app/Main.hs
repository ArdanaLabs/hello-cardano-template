module Main where

import Network.Wai.Handler.Warp (setPort, defaultSettings)
import Network.Wai.Handler.WarpTLS as Warp (runTLS, defaultTlsSettings)
import Options.Applicative

import Network.Kucoin.Server.Mock (kucoinMockApp)
import CLI (Serve(..), mockServerOptions)

main :: IO ()
main = do
  execParser (mockServerOptions "kucoin") >>= \(Serve priceDataPath port) ->
    runTLS defaultTlsSettings (setPort port defaultSettings) (kucoinMockApp priceDataPath)
