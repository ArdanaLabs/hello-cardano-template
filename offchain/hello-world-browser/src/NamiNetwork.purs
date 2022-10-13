module HelloWorld.NamiNetwork where

import Prelude

import Control.Promise (Promise, toAffE)
import Ctl.Internal.Serialization.Address (NetworkId(..))
import Effect (Effect)
import Effect.Aff (Aff, error, throwError)

foreign import _getNetworkId :: Effect (Promise Int)

getNetworkId :: Aff NetworkId
getNetworkId = do
  networkId <- toAffE _getNetworkId
  case networkId of
    0 -> pure TestnetId
    1 -> pure MainnetId
    _ -> throwError $ error "Unknown network ID"
