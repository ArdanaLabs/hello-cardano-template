module Main (main) where

import Hello (helloWorldHexString)

import Control.Monad (unless)
import System.Directory (doesDirectoryExist)
import System.Environment (getArgs)
import System.Exit (die)

main :: IO ()
main = do
  getArgs >>= \case
    [out] -> do
      exists <- doesDirectoryExist out
      unless exists $ die $ "directory: " <> out <> " does not exist"
      writeFile (out ++ "/hello_world.cbor.txt") helloWorldHexString
    _ -> do
      die "usage: cabal run hello-world <file_path>"
