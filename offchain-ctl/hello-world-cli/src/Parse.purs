module Parse(CliCmd(..)) where

import Prelude
import Data.Maybe(Maybe(Nothing,Just))
import Data.String.Utils(words)
import Data.Show.Generic (genericShow)
import Data.Generic.Rep (class Generic)
import Data.Int(fromString)
import Effect.Aff(Aff)
import Effect.Class (liftEffect)

data CliCmd
  = Start
    {contractParam :: Int
    ,initialDatum :: Int
    }
  | SetDatum
    {newDatum :: Int
    }
  | End

derive instance genericCliCmd :: Generic CliCmd _

instance showCliCmd :: Show CliCmd where
  show = genericShow

{-
getCmd :: Aff CliCmd
getCmd = do
  interface <- liftEffect $ createConsoleInterface noCompletion
  (cmdStr :: String) <- question "hello-world-cli$" interface
  case parseCommand cmdStr of
    Just cmd -> pure cmd
    Nothing -> do
      -- TODO warn user that the parse failed
      getCmd
-}

parseCommand :: String -> Maybe CliCmd
parseCommand cmd = case words cmd of
  ["Start",param,initial] ->
    (compose >>> compose) Start {contractParam:_,initialDatum:_}
      <$> fromString param
      <*> fromString initial
  ["SetDatum",new] ->
    SetDatum <<< {newDatum:_}
      <$> fromString new
  ["End"] -> pure End
  _ -> Nothing
