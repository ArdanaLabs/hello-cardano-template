module Test.Main
  ( main
  , _main
  ) where

import Prelude
import Effect (Effect)
import Test.Encoding as Encoding
import Test.HelloWorld.Api as Test.HelloWorld.Api
import Effect.Class.Console as Console
import Effect.Aff (launchAff_)
import Test.Spec.Runner (runSpec', defaultConfig)
import Test.Spec.Reporter.Console (consoleReporter)
import Data.Maybe (Maybe(Nothing))
import Signing as Signing


_main :: Effect Unit
_main = do
  launchAff_ $ runSpec' defaultConfig { timeout = Nothing } [ consoleReporter ] do
    Encoding.spec
    Test.HelloWorld.Api.spec

main :: Effect Unit
main = launchAff_ $ do
  res <- Signing.fetch "http://localhost:3000/pubkey"
  Console.log res
  pure unit