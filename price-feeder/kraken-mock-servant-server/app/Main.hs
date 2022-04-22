module Main where

import Network.Wai.Handler.Warp (setPort, defaultSettings)
import Network.Wai.Handler.WarpTLS as Warp (runTLS, defaultTlsSettings)

import Network.Kraken.Server.Mock (krakenMockApp)

main :: IO ()
main = runTLS defaultTlsSettings (setPort 443 defaultSettings) krakenMockApp
