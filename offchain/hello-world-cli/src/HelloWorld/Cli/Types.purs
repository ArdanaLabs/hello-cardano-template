module HelloWorld.Cli.Types
  ( Options(..)
  , Command(..)
  , CliState(..)
  , Conf(..)
  , ParsedOptions(..)
  , ParsedConf
  , FileState
  , WalletConf(..)
  ) where

import Prelude

import Aeson (class DecodeAeson, decodeAeson)
import Contract.Address (NetworkId)
import Contract.Transaction (TransactionInput)
import Contract.Wallet (KeyWallet)
import Control.Alt ((<|>))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)
import Data.UInt (UInt)
import Data.Newtype (class Newtype)

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

newtype Conf = Conf
  { wallet :: KeyWallet
  , network :: NetworkId
  }

type ParsedConf =
  { wallet :: WalletConf
  , network :: String
  }

data WalletConf
  = KeyWalletFiles { walletPath :: String, stakingPath :: Maybe String }
  | YubiHSM { useYubiHSM :: Boolean }

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

instance DecodeAeson WalletConf where
  decodeAeson a =
    (KeyWalletFiles <$> decodeAeson a) <|> (YubiHSM <$> decodeAeson a)

derive instance Generic Conf _
derive instance Newtype Conf _
