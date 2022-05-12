module DUSD (main) where

import Hello (helloValidator)
import Plutarch (printScript)
import Plutus.V1.Ledger.Scripts (getValidator)

main :: IO ()
main = do
  putStrLn $ printScript (getValidator helloValidator)
