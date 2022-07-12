module Main
  ( main
  ) where

import Contract.Prelude
import Contract.Monad ( launchAff_)
import Options.Applicative(execParser)
import Parser(parser)
import Runners(readConfig)

main :: Effect Unit
main = do
  parsedCmd <- execParser parser
  log $ show parsedCmd
  cmd <- readConfig parsedCmd
  log $ show cmd
  launchAff_ $ pure unit
