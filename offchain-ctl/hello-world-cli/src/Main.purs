module Main
  ( main
  ) where

import Contract.Prelude
import Contract.Monad ( launchAff_)
import Options.Applicative(execParser)
import Parser(parser)

main :: Effect Unit
main = do
  parsedCfg <- execParser parser
  log $ show parsedCfg
  launchAff_ $ pure unit
