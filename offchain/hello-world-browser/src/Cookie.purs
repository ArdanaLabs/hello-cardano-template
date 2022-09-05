module KeyWallet.Cookie where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)

getCookie :: String -> Aff (Maybe String)
getCookie key = fromEffectFnAff $ _getCookie key Just Nothing

foreign import _getCookie :: forall a. String -> (a -> Maybe a) -> (Maybe a) -> EffectFnAff (Maybe String)
