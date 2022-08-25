module Main (main) where

import Apropos.Plutus.HelloValidator qualified as HelloValidator
import Goldens.Cbor qualified as Cbor

import Test.Syd
import System.Environment(lookupEnv)
import Data.Maybe(fromMaybe,isNothing)
import Control.Monad(when)

-- TODO use sydtest-discover once nix stabalizes a bit more
-- TODO figure out why sydtest breaks the histograms and fix it

main :: IO ()
main = do
  maybeGoldenDir <- lookupEnv "GOLDEN_FILES"
  when (isNothing maybeGoldenDir) $ putStrLn "env GOLDEN_FILES not set"
  let goldenDir = fromMaybe "./" maybeGoldenDir
  putStrLn goldenDir
  sydTest $ do
    describe "plutus" $ do
      HelloValidator.spec
    describe "goldens" $ do
      Cbor.spec goldenDir
