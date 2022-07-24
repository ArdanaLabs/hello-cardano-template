module Main
  ( main
  ) where

import Contract.Prelude

import Contract.Monad (defaultTestnetContractConfig)
import Effect (Effect)
import Effect.Aff (throwError, try)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import HelloWorld.AppM (runAppM)
import HelloWorld.Page.Home as Home

main :: Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody

    contractConfig <- try defaultTestnetContractConfig
    case contractConfig of
      Left err -> throwError err
      Right cfg -> do
        let
          store =
            { contractConfig: cfg
            , lastOutput: Nothing
            }
        rootComponent <- runAppM store Home.component
        runUI rootComponent unit body
