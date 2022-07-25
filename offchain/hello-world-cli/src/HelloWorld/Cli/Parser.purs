module HelloWorld.Cli.Parser
  (parser
  ) where

import Prelude

import Options.Applicative
  (CommandFields
  ,Mod
  ,Parser
  ,ParserInfo
  ,command
  ,info
  ,int
  ,long
  ,metavar
  ,option
  ,progDesc
  ,short
  ,strOption
  ,hsubparser
  ,fullDesc
  ,header
  ,commandGroup
  ,helper
  )

import HelloWorld.Cli.Types(ParsedOptions(..),Command(..))

parser :: ParserInfo ParsedOptions
parser = info rawParser
  (fullDesc
    <> progDesc "This comand-line tool can be used to automate transactions with the hello-world api"
    <> header "Hello-World cli"
    )

rawParser :: Parser ParsedOptions
rawParser = (helper <*>  _)
  $ map ParsedOptions
  $ {configFile:_,statePath:_,command:_}
  <$> config
  <*> stateFile
  <*> com

config :: Parser String
config = strOption (long "config" <> short 'c' <> metavar "CONFIG_FILE")

stateFile :: Parser String
stateFile = strOption (long "state-file" <> short 's' <> metavar "STATE_FILE")

-- the name command is taken
com :: Parser Command
com = hsubparser $
  (commandGroup "Cli commands:") <>
  lock <> increment <> unlock <> query

lock :: Mod CommandFields Command
lock  = command "lock" (info (Lock <$> lockOptions) (progDesc "lock some ada with the contract"))
  where
    lockOptions :: Parser {contractParam :: Int,initialDatum :: Int}
    lockOptions =
      {contractParam:_,initialDatum:_}
        <$> option int (long "param" <> short 'p' <> metavar "CONTRACT_PARAMETER")
        <*> option int (long "init" <> short 'i' <> metavar "INITIAL_DATUM")

increment :: Mod CommandFields Command
increment = command "increment" (info (pure Increment) (progDesc "increment the datum"))

unlock :: Mod CommandFields Command
unlock = command "unlock" (info (pure Unlock) (progDesc "unlock the value back to your wallet"))

query :: Mod CommandFields Command
query = command "query" (info (pure Query) (progDesc "query info about a state file"))

