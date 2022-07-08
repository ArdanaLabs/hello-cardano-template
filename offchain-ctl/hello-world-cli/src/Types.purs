module Types(Command(..),SubCommand(..),CliState(..),Conf(..)) where

import Contract.Transaction ( TransactionInput)
import Serialization.Address(NetworkId)

data Command = Command
  { subCommand :: SubCommand
  , statePath :: String
  , conf :: Conf
  }

data SubCommand
  = Lock
    {contractParam :: Int
    ,initialDatum :: Int
    }
  | Increment
  | End

data CliState
  = State
  { param :: Int
  , lastOutput :: TransactionInput
  , datum :: Int
  }

data Conf
  = Conf
  { walletPath :: String
  , stakingPath :: String
  , network :: NetworkId
  }
