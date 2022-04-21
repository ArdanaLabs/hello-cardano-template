module DUSD (main) where

import Hello (helloScript)
import Plutarch (printScript)

main :: IO ()
main = do
  putStrLn $ printScript helloScript
