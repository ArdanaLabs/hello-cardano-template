module Main where

import Network.Wai.Handler.Warp (run)
import Options.Applicative

import CLI (Serve (..), mockServerOptions)
import Network.Binance.Server.Mock (binanceMockApp)

main :: IO ()
main = do
  execParser (mockServerOptions "kraken") >>= \(Serve priceDataPath port) ->
    run port (binanceMockApp priceDataPath)
