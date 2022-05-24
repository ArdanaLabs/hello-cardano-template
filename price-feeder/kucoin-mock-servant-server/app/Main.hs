module Main where

import Network.Wai.Handler.Warp (run)
import Options.Applicative

import CLI (Serve (..), mockServerOptions)
import Network.Kucoin.Server.Mock (kucoinMockApp)

main :: IO ()
main = do
  execParser (mockServerOptions "kucoin") >>= \(Serve priceDataPath port) ->
    run port (kucoinMockApp priceDataPath)
