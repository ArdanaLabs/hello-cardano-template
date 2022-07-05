module Main where

import Prelude

import Effect (Effect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import HelloWorld.AppM (runAppM)
import HelloWorld.Env (BaseURL(..), Wallet(..))
import HelloWorld.Page.Home as Home

main :: Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody
    let
      env =
        { baseURL: BaseURL "http://localhost:9080"
        , wallet: Wallet "2d4cc31a4b3116ab86bfe529d30d9c362acd0b44"
        }

      rootComponent = H.hoist (runAppM env) Home.component
    runUI rootComponent unit body
