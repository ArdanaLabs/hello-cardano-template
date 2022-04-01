module DUSD (main) where

import Hello (helloScript)
import Plutus.V1.Ledger.Scripts (Script (Script))

main :: IO ()
main = do
  let (Script hs) = helloScript
  print hs
