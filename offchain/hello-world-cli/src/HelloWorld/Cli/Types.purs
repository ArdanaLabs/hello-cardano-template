module HelloWorld.Cli.Types
  ( Options(..)
  , Command(..)
  , CliState(..)
  , Conf(..)
  , ParsedOptions(..)
  , ParsedConf
  , FileState
  ) where

import Prelude
import Contract.Transaction (TransactionInput)
import Serialization.Address (NetworkId)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Maybe (Maybe)
import Data.UInt (UInt)

data Options = Options
  { command :: Command
  , statePath :: String
  , conf :: Conf
  , ctlPort :: Maybe UInt
  , ogmiosPort :: Maybe UInt
  , odcPort :: Maybe UInt
  }

data Command
  = Lock
      { contractParam :: Int
      , initialDatum :: Int
      }
  | Increment
  | Unlock
  | Query

type FileState =
  { param :: Int
  , lastOutput ::
      { index :: Int
      , transactionId :: String
      }
  }

data CliState = State
  { param :: Int
  , lastOutput :: TransactionInput
  }

data Conf = Conf
  { walletPath :: String
  , stakingPath :: Maybe String
  , network :: NetworkId
  }

type ParsedConf =
  { walletPath :: String
  , stakingPath :: Maybe String
  , network :: String
  }

-- same as Command but the config hasn't been read from a file
data ParsedOptions = ParsedOptions
  { command :: Command
  , statePath :: String
  , configFile :: String
  , ctlPort :: Maybe UInt
  , ogmiosPort :: Maybe UInt
  , odcPort :: Maybe UInt
  }

derive instance Generic Command _
instance Show Command where
  show = genericShow

derive instance Generic ParsedOptions _
instance Show ParsedOptions where
  show = genericShow

derive instance Generic Options _
instance Show Options where
  show = genericShow

derive instance Generic Conf _
instance Show Conf where
  show = genericShow
