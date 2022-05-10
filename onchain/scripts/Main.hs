module Main (main) where

import Codec.Serialise (serialise)
import Data.ByteString.Lazy qualified as BSL
import Hello (helloValidator)
import System.Environment (getArgs)
import Plutus.V1.Ledger.Scripts (getValidator)

main :: IO ()
main = do
  [out] <- getArgs
  BSL.writeFile (out ++ "/hello_world.plc") (serialise (getValidator helloValidator))
