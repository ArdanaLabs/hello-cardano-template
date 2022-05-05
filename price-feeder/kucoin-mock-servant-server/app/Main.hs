module Main where

import Network.Wai.Handler.Warp (setPort, defaultSettings)
import Network.Wai.Handler.WarpTLS as Warp (runTLS, defaultTlsSettings)

import Network.Kucoin.Server.Mock (kucoinMockApp)

main :: IO ()
main = runTLS defaultTlsSettings (setPort 443 defaultSettings) kucoinMockApp
