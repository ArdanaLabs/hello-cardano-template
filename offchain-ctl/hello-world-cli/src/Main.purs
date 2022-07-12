module Main
  ( main
  ) where

import Contract.Prelude
import Effect.Aff(launchAff_)
import Options.Applicative(execParser)
import Parser(parser)
import Runners(readConfig,runCmd)

main :: Effect Unit
main = do
  parsedCmd <- execParser parser
  launchAff_ $
    readConfig parsedCmd >>= runCmd
