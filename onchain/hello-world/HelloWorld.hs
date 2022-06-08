module Main (main) where

import Hello (helloValidator)
import System.Environment (getArgs)
import Utils (validatorToHexString)

main :: IO ()
main = do
  [out] <- getArgs
  writeFile (out ++ "/hello_world.cbor.txt") hellWorldHexString

hellWorldHexString :: String
hellWorldHexString = validatorToHexString helloValidator
