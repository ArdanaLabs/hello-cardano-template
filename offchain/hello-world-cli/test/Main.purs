module Test.Main
  ( main
  ) where

import Contract.Prelude

import CmdUtils (fails, failsSaying, passesSaying)
import Contract.Test.Plutip (PlutipConfig)
import Data.BigInt as BigInt
import Data.String (trim)
import Data.UInt as UInt
import Effect.Aff (launchAff_)
import Effect.Exception (throw)
import Node.FS.Aff (unlink, exists)
import Node.Process (lookupEnv)
import Test.Spec (describe, it)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec', defaultConfig)
import Test.Wallet (withFundedHsmWalletFile, withPlutipWalletFile)

main :: Effect Unit
main = do
  fixturesDir <- fromMaybe "./fixtures" <$> lookupEnv "TEST_RESOURCES"
  let initialAdaAmount = BigInt.fromInt 35_000_000
  let jsonDir = fixturesDir <> "/jsons/"
  let plutipWalletDir = "./" -- TODO get a writeable dir from nix
  usePlutip <- lookupEnv "MODE" >>= case _ of
    Just "local" -> pure true
    Just "testnet" -> do
      -- TODO add this back when we have an API key again
      -- topup "addr_test1qrwdtldyjseyn3k978de87renmp2kt3vcajk65nk543tw865kp7y0evgnnne7ukzhqsmdmyefhpevpepl9p7xpe8zqpsag6004"
      pure false
    Just e -> throw $ "expected local or testnet got: " <> e
    Nothing -> throw "expected MODE to be set"
  let
    -- The first string is the port arguments the second string is the wallet config path
    withEnv :: (String -> String -> Aff Unit) -> Aff Unit
    withEnv f =
      if usePlutip then withPlutipWalletFile config [ initialAdaAmount ] plutipWalletDir f
      else f "" (jsonDir <> "testWalletCfg.json ")
  launchAff_ $ do
    let cli = "hello-world-cli "
    let badConf = jsonDir <> "badWalletCfg.json "
    let state = " script.clistate "
    let badState = jsonDir <> "badState.json "
    -- remove state file if it exists
    hasOldState <- exists (trim state)
    when hasOldState $ unlink (trim state)

    runSpec' defaultConfig { timeout = Nothing } [ consoleReporter ] $ do
      describe "shell sanity checks" do
        it "ls err"
          $ failsSaying "ls bad_path"
              "No such file"
        it "ls pass"
          $ passesSaying "ls -a"
              "."
      describe "help page" do
        it "knows its own name" -- it used to call itself `purs-nix run`

          $ passesSaying
              (cli <> "--help")
              "Usage: hello-world-cli"
      describe "lock" do
        it "fails on no conf"
          $ failsSaying
              (cli <> "-s" <> state <> "lock -i 0 -p 1")
              "Missing: (-c|--config CONFIG_FILE)"
        it "fails on no state"
          $ withEnv
          $ \ports wallet ->
              failsSaying
                (cli <> ports <> "-c" <> wallet <> "lock -i 0 -p 1")
                "Missing: (-s|--state-file STATE_FILE)"
        it "fails on no inc"
          $ withEnv
          $ \ports wallet ->
              fails $ cli <> ports <> "-c" <> wallet <> "-s" <> state <> "lock -p 1"
        it "fails on no param"
          $ withEnv
          $ \ports wallet ->
              fails $ cli <> ports <> "-c" <> wallet <> "-s" <> state <> "lock -i 0"
        it "fails on bad conf"
          $ fails
          $ cli
          <> "-c"
          <> badConf
          <> "-s"
          <> state
          <> "lock -i 0 -p 1"
        -- There's no hard reason this couldn't be made to work without the runtime
        -- but it happens to look up the datum before noticing the state shouldn't exist
        it "fails when state exists"
          $ withEnv
          $ \ports wallet ->
              failsSaying
                (cli <> ports <> "-c" <> wallet <> "-s" <> badState <> "lock -i 0 -p 1")
                "Can't use lock when state file already exists"
      describe "increment" do
        it "fails on no conf"
          $ failsSaying
              (cli <> "-s" <> state <> "increment")
              "Missing: (-c|--config CONFIG_FILE)"
        it "fails on no state"
          $ withEnv
          $ \ports wallet ->
              failsSaying
                (cli <> ports <> "-c" <> wallet <> "increment")
                "Missing: (-s|--state-file STATE_FILE)"
        it "fails when state doesn't exists"
          $ failsSaying
              (cli <> "-c bad_path -s" <> state <> "increment")
              "[Error: ENOENT: no such file or directory, open 'bad_path']"
      describe "unlock" do
        it "fails on no conf"
          $ failsSaying
              (cli <> "-s" <> state <> "unlock")
              "Missing: (-c|--config CONFIG_FILE)"
        it "fails on no state"
          $ withEnv
          $ \ports wallet ->
              failsSaying
                (cli <> ports <> "-c" <> wallet <> "unlock")
                "Missing: (-s|--state-file STATE_FILE)"
        it "fails when state doesn't exists"
          $ failsSaying
              (cli <> "-c bad_path -s" <> state <> "unlock")
              "[Error: ENOENT: no such file or directory, open 'bad_path']"
      describe "query" do
        it "fails on no conf"
          $ failsSaying
              (cli <> "-s" <> state <> "query")
              "Missing: (-c|--config CONFIG_FILE)"
        it "fails on no state"
          $ withEnv
          $ \ports wallet ->
              failsSaying
                (cli <> ports <> "-c" <> wallet <> "query")
                "Missing: (-s|--state-file STATE_FILE)"
        it "fails when state doesn't exists"
          $ failsSaying
              (cli <> "-c bad_path -s" <> state <> "query")
              "[Error: ENOENT: no such file or directory, open 'bad_path']"
      describe "integration test" do
        when usePlutip $ it "HSM wallet works"
          $ withFundedHsmWalletFile config [ initialAdaAmount ] plutipWalletDir
          $ \ports wallet -> do
              passesSaying
                (cli <> ports <> "-c" <> wallet <> "-s" <> state <> "lock -i 0 -p 1")
                "finished"
              unlink $ trim state
              failsSaying
                ("ls" <> state)
                "No such file"
        -- TODO I'm not sure why they aren't saying finished
        it "locks the value"
          $ withEnv
          $ \ports wallet -> do
              passesSaying
                (cli <> ports <> "-c" <> wallet <> "-s" <> state <> "lock -i 0 -p 1")
                "finished"
              unlink $ trim state
              failsSaying
                ("ls" <> state)
                "No such file"
        it "querys the state"
          $ withEnv
          $ \ports wallet -> do
              passesSaying
                (cli <> ports <> "-c" <> wallet <> "-s" <> state <> "lock -i 0 -p 1")
                "finished"
              passesSaying
                (cli <> ports <> "-c" <> wallet <> "-s" <> state <> "query")
                "Current datum:0"
              unlink $ trim state
              failsSaying
                ("ls" <> state)
                "No such file"
        it "increments the datum"
          $ withEnv
          $ \ports wallet -> do
              passesSaying
                (cli <> ports <> "-c" <> wallet <> "-s" <> state <> "lock -i 0 -p 1")
                "finished"
              passesSaying
                (cli <> ports <> "-c" <> wallet <> "-s" <> state <> "query")
                "Current datum:0"
              passesSaying
                (cli <> ports <> "-c" <> wallet <> "-s" <> state <> "increment")
                "finished"
              unlink $ trim state
              failsSaying
                ("ls" <> state)
                "No such file"
        it "querys the new state"
          $ withEnv
          $ \ports wallet -> do
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
              unlink $ trim state
              failsSaying
                ("ls" <> state)
                "No such file"
        it "it unlocks the value"
          $ withEnv
          $ \ports wallet -> do
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
        it "removed the state file"
          $ withEnv
          $ \ports wallet -> do
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
  , port: UInt.fromInt 8086
  , logLevel: Warn
  -- Server configs are used to deploy the corresponding services.
  , ogmiosConfig:
      { port: UInt.fromInt 1339
      , host: "127.0.0.1"
      , secure: false
      , path: Nothing
      }
  , ogmiosDatumCacheConfig:
      { port: UInt.fromInt 10001
      , host: "127.0.0.1"
      , secure: false
      , path: Nothing
      }
  , ctlServerConfig: Just
      { port: UInt.fromInt 8085
      , host: "127.0.0.1"
      , secure: false
      , path: Nothing
      }
  , postgresConfig:
      { host: "127.0.0.1"
      , port: UInt.fromInt 5434
      , user: "ctxlib"
      , password: "ctxlib"
      , dbname: "ctxlib"
      }
  , customLogger: Nothing
  , suppressLogs: false
  }
