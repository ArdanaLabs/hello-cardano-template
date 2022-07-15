module Types
  (Command(..)
  ,SubCommand(..)
  ,CliState(..)
  ,Conf(..)
  ,ParsedOptions(..)
  ,ParsedConf
  ,FileState
  ) where

import Prelude
import Contract.Transaction ( TransactionInput)
import Serialization.Address(NetworkId)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic(genericShow)

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
  | Querry

type FileState =
  { param :: Int
  , lastOutput ::
    { index :: Int
    , transactionId :: String
    }
  }

data CliState
  = State
  { param :: Int
  , lastOutput :: TransactionInput
  }

data Conf
  = Conf
  { walletPath :: String
  , stakingPath :: String
  , network :: NetworkId
  }

type ParsedConf
  =
  { walletPath :: String
  , stakingPath :: String
  , network :: String
  }

-- same as Command but the config hasn't been read from a file
data ParsedOptions = ParsedOptions
  { subCommand :: SubCommand
  , statePath :: String
  , configFile :: String
  }

-- I don't get why the instances have names
-- or why there is so much boiler plate
-- maybe these names should be better
-- I don't know if it matters

derive instance a1 :: Generic Command _
instance a2 :: Show Command where
  show = genericShow

derive instance b1 :: Generic ParsedOptions _
instance b2 :: Show ParsedOptions where
  show = genericShow

derive instance c1 :: Generic SubCommand _
instance c2 :: Show SubCommand where
  show = genericShow

derive instance d1 :: Generic Conf _
instance d2 :: Show Conf where
  show = genericShow
