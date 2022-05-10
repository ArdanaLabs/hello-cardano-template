module Main where

import Prelude

import Effect (Effect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import HelloWorld.AppM (runAppM)
import HelloWorld.Env (BaseURL(..), ContractID(..))
import HelloWorld.Page.Home as Home

main :: Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody
    let
      env =
        { baseURL: BaseURL "http://localhost:9080"
        , contractID: ContractID "8a2d774e-c736-4085-b100-47c7eb7b4123"
        }

      rootComponent = H.hoist (runAppM env) Home.component
    runUI rootComponent unit body
