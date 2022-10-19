module HelloWorld.CardanoApi where

import Prelude

import Control.Promise (Promise, toAffE)
import Ctl.Internal.Serialization.Address (NetworkId(..))
import Ctl.Internal.Wallet (Cip30Connection)
import Effect (Effect)
import Effect.Aff (Aff, error, throwError)

foreign import _getNetworkId :: Cip30Connection -> Effect (Promise Int)
foreign import _enable :: String -> Effect (Promise Cip30Connection)

getNetworkId :: String -> Aff NetworkId
getNetworkId wallet = do
  conn <- enable wallet
  networkId <- toAffE $ _getNetworkId conn
  case networkId of
    0 -> pure TestnetId
    1 -> pure MainnetId
    _ -> throwError $ error "Unknown network ID"

enable :: String -> Aff Cip30Connection
enable = toAffE <<< _enable
