module Main where

import Network.Wai.Handler.Warp (run)
import Options.Applicative

import CLI (Serve (..), mockServerOptions)
import Network.Kraken.Server.Mock (krakenMockApp)

main :: IO ()
main = do
  execParser (mockServerOptions "kraken") >>= \(Serve priceDataPath port) ->
    run port (krakenMockApp priceDataPath)
