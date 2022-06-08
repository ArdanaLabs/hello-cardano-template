module Main (main) where

import Hello (helloWorldHexString)
import System.Environment (getArgs)

main :: IO ()
main = do
  [out] <- getArgs
  writeFile (out ++ "/hello_world.cbor.txt") helloWorldHexString
