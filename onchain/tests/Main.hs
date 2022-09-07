module Main (main) where

import Apropos.Plutus.HelloValidator qualified as HelloValidator
import Goldens.Cbor qualified as Cbor

import Control.Monad (when)
import Data.Maybe (fromMaybe, isNothing)
import System.Environment (lookupEnv)
import Test.Syd

-- TODO use sydtest-discover once nix stabalizes a bit more
-- TODO figure out why sydtest breaks the histograms and fix it

main :: IO ()
main = do
  maybeGoldenDir <- lookupEnv "GOLDEN_FILES"
  when (isNothing maybeGoldenDir) $ putStrLn "env GOLDEN_FILES not set"
  let goldenDir = fromMaybe "./goldens/" maybeGoldenDir
  putStrLn goldenDir
  sydTest $ do
    describe "plutus" $ do
      HelloValidator.spec
    describe "goldens" $ do
      Cbor.spec goldenDir
