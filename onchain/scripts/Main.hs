module Main (main) where

import Codec.Serialise (serialise)
import Data.ByteString.Lazy qualified as BSL
import Hello (helloScript)
import System.Environment (getArgs)

main :: IO ()
main = do
  [out] <- getArgs
  BSL.writeFile (out ++ "/hello_world.plc") (serialise helloScript)
