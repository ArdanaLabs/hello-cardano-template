module Main where

import Network.Wai.Handler.Warp (run)
import Options.Applicative

import CLI (Serve (..), mockServerOptions)
import Network.Huobi.Server.Mock (huobiMockApp)

main :: IO ()
main = do
  execParser (mockServerOptions "huobi") >>= \(Serve priceDataPath port) ->
    run port (huobiMockApp priceDataPath)
