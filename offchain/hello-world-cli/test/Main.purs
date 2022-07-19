module Test.Main
  ( main
  ) where

import Contract.Prelude
import Effect.Aff(launchAff_)
import CmdUtils(fails,passes,spawnAff)
import Test.Spec(it,describe)
import Test.Spec.Runner (runSpec',defaultConfig)
import Test.Spec.Reporter.Console (consoleReporter)

main :: Effect Unit
main = launchAff_ $ do
  _ <- spawnAff "nix build .#hello-world-cli"
  let cli = "./result/bin/hello-world-cli "
  runSpec' defaultConfig{timeout=Nothing} [ consoleReporter ] do
    describe "shell sanity checks" do
      it "ls err"
        $ fails "ls bad_path"
      it "ls pass"
        $ passes "ls -a"
    describe "lock" do
      it "fails no conf"
        $ fails $ cli <> "-s state lock -i 0 -p 1"
      it "fails no state"
        $ fails $ cli <> "-c conf lock -i 0 -p 1"

