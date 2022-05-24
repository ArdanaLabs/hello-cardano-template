module Main where

import Network.Wai.Handler.Warp (run)
import Options.Applicative

import CLI (Serve (..), mockServerOptions)
import Network.Coinbase.Server.Mock (coinbaseMockApp)

main :: IO ()
main = do
  execParser (mockServerOptions "coinbase") >>= \(Serve priceDataPath port) ->
    run port (coinbaseMockApp priceDataPath)
