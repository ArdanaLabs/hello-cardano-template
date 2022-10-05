module Onchain (main) where

import Hello (helloValidator)
import Plutarch (printScript)
import PlutusLedgerApi.V1.Scripts (getValidator)

main :: IO ()
main = do
  putStrLn $ printScript (getValidator helloValidator)
