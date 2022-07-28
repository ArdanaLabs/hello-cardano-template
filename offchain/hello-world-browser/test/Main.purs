module Test.Main
  ( main
  ) where

import Contract.Prelude

import Contract.Config (testnetConfig)
import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import HelloWorld.AppM (runAppM)
import HelloWorld.Page.Home as Home
import Test.KeyWallet (loadKeyWallet)

main :: Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody
    walletSpec <- loadKeyWallet
    let contractConfig = testnetConfig { walletSpec = Just walletSpec }
    let
      store =
        { contractConfig
        , lastOutput: Nothing
        }
    rootComponent <- runAppM store Home.component
    runUI rootComponent unit body