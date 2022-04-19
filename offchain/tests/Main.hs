module Main (main) where

import Codec.Serialise (deserialise)
import Data.ByteString.Lazy qualified as BSL
import Plutus.V1.Ledger.Scripts (Script, scriptSize)
import System.Environment (getEnv)
import System.Exit (exitFailure, exitSuccess)

main :: IO ()
main = do
  s <- getEnv "DUSD_SCRIPTS" >>= BSL.readFile . (++ "/hello_world.plc")
  let (sc :: Script) = deserialise s
  if scriptSize sc == 459
    then exitSuccess
    else do
      print $ scriptSize sc
      exitFailure
