module Test.Main
  ( main
  ) where

import Prelude

import Data.Maybe (Maybe(Nothing))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Encoding as Encoding
import Test.HelloWorld.Api as Test.HelloWorld.Api
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec', defaultConfig)


main :: Effect Unit
main = do
  launchAff_ $ runSpec' defaultConfig { timeout = Nothing } [ consoleReporter ] do
    Encoding.spec
    Test.HelloWorld.Api.spec
