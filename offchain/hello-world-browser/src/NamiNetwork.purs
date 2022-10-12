module HelloWorld.NamiNetwork where

import Prelude

import Ctl.Internal.Serialization.Address (NetworkId(..))
import Effect.Aff (Aff, error, throwError)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)

getNetworkId :: Aff NetworkId
getNetworkId = do
  fromEffectFnAff _getNetworkId >>= case _ of
    0 -> pure TestnetId
    1 -> pure MainnetId
    _ -> throwError $ error "Unknown network ID"

foreign import _getNetworkId :: EffectFnAff Int
