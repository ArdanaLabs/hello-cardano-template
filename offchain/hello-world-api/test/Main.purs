module Test.Main
  (main
  ) where

import Prelude
import Effect (Effect)
import Test.Encoding as Encoding
import KeyWallet as KeyWallet

import Effect.Aff (launchAff_)
import Node.Process(lookupEnv)
import Test.Spec.Runner (runSpec',defaultConfig)
import Test.Spec.Reporter.Console (consoleReporter)
import Data.Maybe(Maybe(Nothing),isNothing)

main :: Effect Unit
main = do
  runtime <- isNothing <$> lookupEnv "NO_RUNTIME"
  launchAff_ $ runSpec' defaultConfig{timeout=Nothing} [ consoleReporter ] do
    Encoding.spec
    when runtime $ KeyWallet.spec
