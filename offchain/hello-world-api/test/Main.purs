module Test.Main
  (main
  ) where

import Prelude
import Effect (Effect)
import Test.Encoding as Encoding
import KeyWallet as KeyWallet

import Effect.Aff (launchAff_)
import Test.Spec.Runner (runSpec',defaultConfig)
import Test.Spec.Reporter.Console (consoleReporter)
import Data.Maybe(Maybe(Nothing))

main :: Effect Unit
main = launchAff_ $ runSpec' defaultConfig{timeout=Nothing} [ consoleReporter ] do
  Encoding.spec
  KeyWallet.spec
