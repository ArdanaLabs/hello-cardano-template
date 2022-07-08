module Parser(parser) where

import Prelude

import Options.Applicative
  (Parser
  ,strOption
  ,long
  ,short
  ,metavar
  )
import Data.Tuple.Nested((/\),type (/\))

parser :: Parser (String /\ String)
parser = (/\) <$> config <*> stateFile

config :: Parser String
config = strOption (long "config" <> short 'c' <> metavar "CONFIG")

stateFile :: Parser String
stateFile = strOption (long "state-file" <> short 's' <> metavar "STATE_FILE")
