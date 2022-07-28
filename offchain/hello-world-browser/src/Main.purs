module Main
  ( main
  ) where

import Contract.Prelude

import Contract.Config (testnetNamiConfig)
import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import HelloWorld.AppM (runAppM)
import HelloWorld.Page.Home as Home

main :: Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody
    let contractConfig = testnetNamiConfig { logLevel = Warn }
    let
      store =
        { contractConfig
        , lastOutput: Nothing
        }
    rootComponent <- runAppM store Home.component
    runUI rootComponent unit body

