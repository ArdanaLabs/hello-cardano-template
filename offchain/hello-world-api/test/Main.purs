module Test.Main
  (main
  ) where

import Prelude
import Effect (Effect)
import Effect.Console (logShow)
import Test.Encoding as Encoding

import Effect.Aff (launchAff_)
import Test.Spec.Runner (runSpec',defaultConfig)
import Test.Spec.Reporter.Console (consoleReporter)
import Data.Maybe(Maybe(Nothing),isNothing)
import Test.HelloWorld.Api as Test.HelloWorld.Api

main :: Effect Unit
main = do
  launchAff_ $ runSpec' defaultConfig{timeout=Nothing} [ consoleReporter ] do
    Encoding.spec
    Test.HelloWorld.Api.spec
