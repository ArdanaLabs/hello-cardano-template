module Parser
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
  ,subparser
  ,fullDesc
  ,header
  )
import Types(ParsedOptions(..),SubCommand(..))


-- TODO better help text in general

parser :: ParserInfo ParsedOptions
parser = info rawParser
  (fullDesc
    <> progDesc "This command can be used to automate transactions with the hello-world api"
    <> header "Hello-World cli"
    )

rawParser :: Parser ParsedOptions
rawParser = (ParsedOptions <$> _) $ {configFile:_,statePath:_,subCommand:_}
  <$> config
  <*> stateFile
  <*> subCommand

config :: Parser String
config = strOption (long "config" <> short 'c' <> metavar "CONFIG_FILE")

stateFile :: Parser String
stateFile = strOption (long "state-file" <> short 's' <> metavar "STATE_FILE")

subCommand :: Parser SubCommand
subCommand = subparser $ lock <> increment <> end

lock :: Mod CommandFields SubCommand
lock  = command "lock" (info (Lock <$> lockOptions) (progDesc "lock some ada with the contract"))
  where
    lockOptions :: Parser {contractParam :: Int,initialDatum :: Int}
    lockOptions =
      {contractParam:_,initialDatum:_}
        <$> option int (long "param" <> short 'p' <> metavar "CONTRACT_PARAMETER")
        <*> option int (long "init" <> short 'i' <> metavar "INITIAL_DATUM")

increment ::  Mod CommandFields SubCommand
increment = command "inc" (info (pure Increment) (progDesc "lock some ada with the contract"))

end ::  Mod CommandFields SubCommand
end = command "end" (info (pure End) (progDesc "lock some ada with the contract"))

