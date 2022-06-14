module Main (main) where

import Hello

import Control.Monad (unless)
import System.Directory (doesDirectoryExist)
import System.Environment (getArgs)
import System.Exit (die)

{- | Main takes a directory as a comand line argument
 and puts the cbor hex string of the hello world validator
 into a file named `hello_world.cbor.txt` in that directory.
-}
main :: IO ()
main = do
  getArgs >>= \case
    [out] -> do
      exists <- doesDirectoryExist out
      unless exists $ die $ "directory: " <> out <> " does not exist"
      writeFile (out ++ "/hello_world.cbor.txt") helloWorldHexString
    _ -> do
      die "usage: cabal run hello-world <file_path>"

-- TODO
-- we may end up with more executables very similar to this
-- if so we should make a helper function
