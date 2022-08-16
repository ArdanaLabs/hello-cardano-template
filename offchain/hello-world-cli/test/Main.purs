module Test.Main
  ( main
  ) where

import CmdUtils(fails,failsSaying,passesSaying)
import Contract.Prelude
import Contract.Test.Plutip (PlutipConfig, withPlutipContractEnv)
import Data.String(trim)
import Effect.Aff(launchAff_)
import Node.Process(lookupEnv)
import Node.FS.Aff(unlink)
import Test.Spec(it,describe)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec',defaultConfig)
import Test.Wallet(withPlutipWalletFile)

import Data.BigInt as BigInt
import Data.UInt as UInt

main :: Effect Unit
main = do
  fixturesDir <- fromMaybe "./fixtures" <$> lookupEnv "TEST_RESOURCES"
  let initialAdaAmount = BigInt.fromInt 20_000_000
  let jsonDir = fixturesDir <> "/jsons/"
  let plutipWalletDir = "./" -- TODO get a writeable dir from nix
  launchAff_ $ do
    let cli = "hello-world-cli "
    let badConf = jsonDir <> "badWalletCfg.json "
    let state = " script.clistate "
    let badState = jsonDir <> "badState.json "
    runSpec' defaultConfig{timeout=Nothing} [ consoleReporter ] $ do
      describe "shell sanity checks" do
        it "ls err"
          $ failsSaying "ls bad_path"
            "No such file"
        it "ls pass"
          $ passesSaying "ls -a"
            "."
      describe "help page" do
        it "knows its own name" -- it used to call itself `purs-nix run`
          $ withPlutipWalletFile config [ initialAdaAmount ] plutipWalletDir $ \ports _wallet ->
            passesSaying
            (cli <> ports <> "--help")
            "Usage: hello-world-cli"
      describe "lock" do
        it "fails on no conf"
          $ withPlutipWalletFile config [ initialAdaAmount ] plutipWalletDir $ \ports _wallet ->
            failsSaying
            (cli <> ports <> "-s" <> state <> "lock -i 0 -p 1")
            "Missing: (-c|--config CONFIG_FILE)"
        it "fails on no state"
          $ withPlutipWalletFile config [ initialAdaAmount ] plutipWalletDir $ \ports wallet ->
            failsSaying
            (cli <> ports <> "-c" <> wallet <> "lock -i 0 -p 1")
            "Missing: (-s|--state-file STATE_FILE)"
        it "fails on no inc"
          $ withPlutipWalletFile config [ initialAdaAmount ] plutipWalletDir $ \ports wallet ->
            fails $ cli <> ports <> "-c" <> wallet <> "-s" <> state <> "lock -p 1"
        it "fails on no param"
          $ withPlutipWalletFile config [ initialAdaAmount ] plutipWalletDir $ \ports wallet ->
            fails $ cli <> ports <> "-c" <> wallet <> "-s" <> state <> "lock -i 0"
        it "fails on bad conf"
          $ withPlutipWalletFile config [ initialAdaAmount ] plutipWalletDir $ \ports wallet ->
            fails $ cli <> ports <> "-c" <> badConf <> "-s" <> state <> "lock -i 0 -p 1"
        -- There's no hard reason this couldn't be made to work without the runtime
        -- but it happens to look up the datum before noticing the state shouldn't exist
        it "fails when state exists"
          $ withPlutipWalletFile config [ initialAdaAmount ] plutipWalletDir $ \ports wallet ->
            failsSaying
            (cli <> ports <> "-c" <> wallet <> "-s" <> badState <> "lock -i 0 -p 1")
            "Can't use lock when state file already exists"
      describe "increment" do
        it "fails on no conf"
          $ withPlutipWalletFile config [ initialAdaAmount ] plutipWalletDir $ \ports wallet ->
            failsSaying
            (cli <> ports <> "-s" <> state <> "increment")
            "Missing: (-c|--config CONFIG_FILE)"
        it "fails on no state"
          $ withPlutipWalletFile config [ initialAdaAmount ] plutipWalletDir $ \ports wallet ->
            failsSaying
            (cli <> ports <> "-c" <> wallet <> "increment")
            "Missing: (-s|--state-file STATE_FILE)"
        it "fails when state doesn't exists"
          $ withPlutipWalletFile config [ initialAdaAmount ] plutipWalletDir $ \ports wallet ->
            failsSaying
            (cli <> ports <> "-c bad_path -s" <> state <> "increment")
            "[Error: ENOENT: no such file or directory, open 'bad_path']"
      describe "unlock" do
        it "fails on no conf"
          $ withPlutipWalletFile config [ initialAdaAmount ] plutipWalletDir $ \ports wallet ->
            failsSaying
            (cli <> ports <> "-s" <> state <> "unlock")
            "Missing: (-c|--config CONFIG_FILE)"
        it "fails on no state"
          $ withPlutipWalletFile config [ initialAdaAmount ] plutipWalletDir $ \ports wallet ->
            failsSaying
            (cli <> ports <> "-c" <> wallet <> "unlock")
            "Missing: (-s|--state-file STATE_FILE)"
        it "fails when state doesn't exists"
          $ withPlutipWalletFile config [ initialAdaAmount ] plutipWalletDir $ \ports wallet ->
            failsSaying
            (cli <> ports <> "-c bad_path -s" <> state <> "unlock")
            "[Error: ENOENT: no such file or directory, open 'bad_path']"
      describe "query" do
        it "fails on no conf"
          $ withPlutipWalletFile config [ initialAdaAmount ] plutipWalletDir $ \ports wallet ->
            failsSaying
            (cli <> ports <> "-s" <> state <> "query")
            "Missing: (-c|--config CONFIG_FILE)"
        it "fails on no state"
          $ withPlutipWalletFile config [ initialAdaAmount ] plutipWalletDir $ \ports wallet ->
            failsSaying
            (cli <> ports <> "-c" <> wallet <> "query")
            "Missing: (-s|--state-file STATE_FILE)"
        it "fails when state doesn't exists"
          $ withPlutipWalletFile config [ initialAdaAmount ] plutipWalletDir $ \ports wallet ->
            failsSaying
            (cli <> ports <> "-c bad_path -s" <> state <> "query")
            "[Error: ENOENT: no such file or directory, open 'bad_path']"
      describe "integration test" do
        -- TODO I'm not sure why they aren't saying finished
        it "locks the value"
          $ withPlutipWalletFile config [ initialAdaAmount ] plutipWalletDir $ \ports wallet -> do
            passesSaying
              (cli <> ports <> "-c" <> wallet <> "-s" <> state <> "lock -i 0 -p 1")
              "finished"
            unlink state
            failsSaying
              ("ls" <> state)
              "No such file"
        it "querys the state"
          $ withPlutipWalletFile config [ initialAdaAmount ] plutipWalletDir $ \ports wallet -> do
            passesSaying
              (cli <> ports <> "-c" <> wallet <> "-s" <> state <> "lock -i 0 -p 1")
              "finished"
            passesSaying
              (cli <> ports <> "-c" <> wallet <> "-s" <> state <> "query")
              "Current datum:0"
            unlink state
            failsSaying
              ("ls" <> state)
              "No such file"
        it "increments the datum"
          $ withPlutipWalletFile config [ initialAdaAmount ] plutipWalletDir $ \ports wallet -> do
            passesSaying
              (cli <> ports <> "-c" <> wallet <> "-s" <> state <> "lock -i 0 -p 1")
              "finished"
            passesSaying
              (cli <> ports <> "-c" <> wallet <> "-s" <> state <> "query")
              "Current datum:0"
            passesSaying
              (cli <> ports <> "-c" <> wallet <> "-s" <> state <> "increment")
              "finished"
            unlink state
            failsSaying
              ("ls" <> state)
              "No such file"
        it "querys the new state"
          $ withPlutipWalletFile config [ initialAdaAmount ] plutipWalletDir $ \ports wallet -> do
            passesSaying
              (cli <> ports <> "-c" <> wallet <> "-s" <> state <> "lock -i 0 -p 1")
              "finished"
            passesSaying
              (cli <> ports <> "-c" <> wallet <> "-s" <> state <> "query")
              "Current datum:0"
            passesSaying
              (cli <> ports <> "-c" <> wallet <> "-s" <> state <> "increment")
              "finished"
            passesSaying
              (cli <> ports <> "-c" <> wallet <> "-s" <> state <> "query")
              "Current datum:1"
            unlink state
            failsSaying
              ("ls" <> state)
              "No such file"
        it "it unlocks the value"
          $ withPlutipWalletFile config [ initialAdaAmount ] plutipWalletDir $ \ports wallet -> do
            passesSaying
              (cli <> ports <> "-c" <> wallet <> "-s" <> state <> "lock -i 0 -p 1")
              "finished"
            passesSaying
              (cli <> ports <> "-c" <> wallet <> "-s" <> state <> "query")
              "Current datum:0"
            passesSaying
              (cli <> ports <> "-c" <> wallet <> "-s" <> state <> "increment")
              "finished"
            passesSaying
              (cli <> ports <> "-c" <> wallet <> "-s" <> state <> "query")
              "Current datum:1"
            passesSaying
              (cli <> ports <> "-c" <> wallet <> "-s" <> state <> "unlock")
              "finished"
            unlink state
            failsSaying
              ("ls" <> state)
              "No such file"
        it "removed the state file"
          $ withPlutipWalletFile config [ initialAdaAmount ] plutipWalletDir $ \ports wallet -> do
            passesSaying
              (cli <> ports <> "-c" <> wallet <> "-s" <> state <> "lock -i 0 -p 1")
              "finished"
            passesSaying
              (cli <> ports <> "-c" <> wallet <> "-s" <> state <> "query")
              "Current datum:0"
            passesSaying
              (cli <> ports <> "-c" <> wallet <> "-s" <> state <> "increment")
              "finished"
            passesSaying
              (cli <> ports <> "-c" <> wallet <> "-s" <> state <> "query")
              "Current datum:1"
            passesSaying
              (cli <> ports <> "-c" <> wallet <> "-s" <> state <> "unlock")
              "finished"
            failsSaying
              ("ls" <> state)
              "No such file"

config :: PlutipConfig
config =
  { host: "127.0.0.1"
  , port: UInt.fromInt 8082
  , logLevel: Error
  -- Server configs are used to deploy the corresponding services.
  , ogmiosConfig:
      { port: UInt.fromInt 1338
      , host: "127.0.0.1"
      , secure: false
      , path: Nothing
      }
  , ogmiosDatumCacheConfig:
      { port: UInt.fromInt 10000
      , host: "127.0.0.1"
      , secure: false
      , path: Nothing
      }
  , ctlServerConfig:
      { port: UInt.fromInt 8083
      , host: "127.0.0.1"
      , secure: false
      , path: Nothing
      }
  , postgresConfig:
      { host: "127.0.0.1"
      , port: UInt.fromInt 5433
      , user: "ctxlib"
      , password: "ctxlib"
      , dbname: "ctxlib"
      }
  }
