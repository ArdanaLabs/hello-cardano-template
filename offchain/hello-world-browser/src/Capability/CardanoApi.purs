module HelloWorld.Capability.CardanoApi where

import Prelude

import Data.Either (Either)
import Halogen (HalogenM, lift)
import HelloWorld.Error (HelloWorldBrowserError)
import HelloWorld.Types (HelloWorldWallet)

class Monad m <= CardanoApi m where
  enable :: HelloWorldWallet -> m (Either HelloWorldBrowserError Unit)

instance cardanoApiHalogenM :: CardanoApi m => CardanoApi (HalogenM st act slots msg m) where
  enable = lift <<< enable
