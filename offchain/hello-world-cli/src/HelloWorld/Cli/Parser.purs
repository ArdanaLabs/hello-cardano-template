module HelloWorld.Cli.Parser
  ( parser
  ) where

import Prelude

import Data.Maybe (Maybe(Nothing, Just))
import Data.UInt (UInt, fromInt)
import Options.Applicative
  ( CommandFields
  , Mod
  , Parser
  , ParserInfo
  , command
  , info
  , int
  , long
  , metavar
  , option
  , progDesc
  , short
  , strOption
  , hsubparser
  , fullDesc
  , header
  , commandGroup
  , helper
  , help
  , value
  )

import HelloWorld.Cli.Types (ParsedOptions(..), Command(..))

parser :: ParserInfo ParsedOptions
parser = info rawParser
  ( fullDesc
      <> progDesc "This comand-line tool can be used to automate transactions with the hello-world api"
      <> header "Hello-World cli"
  )

rawParser :: Parser ParsedOptions
rawParser = (helper <*> _)
  $ map ParsedOptions
  $ { configFile: _, statePath: _, command: _, ctlPort: _, ogmiosPort: _, odcPort: _ }
      <$> config
      <*> stateFile
      <*> com
      <*> ctlPort
      <*> ogmiosPort
      <*> odcPort

config :: Parser String
config = strOption $
  long "config"
    <> short 'c'
    <> metavar "CONFIG_FILE"
    <> help "the config file for the wallet to be used"

stateFile :: Parser String
stateFile = strOption $
  long "state-file"
    <> short 's'
    <> metavar "STATE_FILE"
    <> help "the path to the state file to be used"

ctlPort :: Parser (Maybe UInt)
ctlPort = option (Just <<< fromInt <$> int) $
  long "ctl-port"
    <> metavar "CTL_PORT"
    <> value Nothing
    <> help "The port number for the ctl-server. You usually don't need to change this"

ogmiosPort :: Parser (Maybe UInt)
ogmiosPort = option (Just <<< fromInt <$> int) $
  long "ogmios-port"
    <> metavar "OGMIOS_PORT"
    <> value Nothing
    <> help "The port number for ogmios. You usually don't need to change this"

odcPort :: Parser (Maybe UInt)
odcPort = option (Just <<< fromInt <$> int) $
  long "odc-port"
    <> metavar "ODC_PORT"
    <> value Nothing
    <> help "The port number for the ogmios datum cache. You usually don't need to change this"

-- the name command is taken
com :: Parser Command
com = hsubparser $
  (commandGroup "Cli commands:")
    <> lock
    <> increment
    <> unlock
    <> query

lock :: Mod CommandFields Command
lock = command "lock" (info (Lock <$> lockOptions) (progDesc "lock some ada with the contract"))
  where
  lockOptions :: Parser { contractParam :: Int, initialDatum :: Int }
  lockOptions =
    { contractParam: _, initialDatum: _ }
      <$> option int (long "param" <> short 'p' <> metavar "CONTRACT_PARAMETER")
      <*> option int (long "init" <> short 'i' <> metavar "INITIAL_DATUM")

increment :: Mod CommandFields Command
increment = command "increment" (info (pure Increment) (progDesc "increment the datum"))

unlock :: Mod CommandFields Command
unlock = command "unlock" (info (pure Unlock) (progDesc "unlock the value back to your wallet"))

query :: Mod CommandFields Command
query = command "query" (info (pure Query) (progDesc "query info about a state file"))

