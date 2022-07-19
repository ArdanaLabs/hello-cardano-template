module Test.Main
  ( main
  ) where

import Contract.Prelude
import Effect.Aff(launchAff_)
import CmdUtils(fails,passes)
import Test.Spec(it)
import Test.Spec.Runner (runSpec',defaultConfig)
import Test.Spec.Reporter.Console (consoleReporter)

main :: Effect Unit
main = launchAff_ $
  runSpec' defaultConfig{timeout=Nothing} [ consoleReporter ] do
  it "ls fails on bad_path" $ fails "ls bad_path"
  it "ls -a passes" $ passes "ls -a"
  pure unit

