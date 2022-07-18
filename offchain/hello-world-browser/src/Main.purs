module Main
  ( main
  ) where

import AppM (runAppM)
import Contract.Monad (defaultTestnetContractConfig)
import Contract.Prelude
import Effect (Effect)
import Effect.Aff (throwError, try)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Page.Home as Home

main :: Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody
    contractConfig <- try defaultTestnetContractConfig
    case contractConfig of
      Left err -> throwError err
      Right cfg -> do
        let
          env =
            { contractConfig: cfg
            }
          rootComponent = H.hoist (runAppM env) Home.component
        runUI rootComponent unit body
