module Main
  ( main
  , testMain
  ) where

import Prelude

import Contract.Monad (defaultTestnetContractConfig)
import Data.Maybe (Maybe(Nothing))
import Effect (Effect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import HelloWorld.AppM (runAppM)
import HelloWorld.Page.Home as Home
import HelloWorld.TestM (runTestM)

main :: Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody

    contractConfig <- defaultTestnetContractConfig
    let
      store =
        { contractConfig
        , lastOutput: Nothing
        }
    rootComponent <- runAppM store Home.component
    runUI rootComponent unit body

testMain :: Effect Unit
testMain =
  HA.runHalogenAff do
    body <- HA.awaitBody

    let rootComponent = H.hoist runTestM Home.component
    runUI rootComponent unit body