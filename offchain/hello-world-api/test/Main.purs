module Test.Main
  ( main
  ) where

import Prelude

import Data.Maybe (Maybe(Just, Nothing))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Exception (throw)
import Node.Process (lookupEnv)
import Test.HelloWorld.Api as Test.HelloWorld.Api
import Test.HelloWorld.Discovery.Api as Test.HelloWorld.Discovery.Api
import Test.HelloWorld.Encoding as Encoding
import Test.HelloWorld.EnvRunner (Mode(..), getEnvRunner)
import Test.HelloWorld.Signing as Test.HelloWorld.Signing
import Test.Spec (describe)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec', defaultConfig)
import Test.Volume.HelloWorld.Api as Test.Volume.HelloWorld.Api

main :: Effect Unit
main = do
  runVolumeTests <- lookupEnv "RUN_VOLUME_TESTS" >>= case _ of
    Nothing -> pure false
    Just "0" -> pure false
    Just _ -> pure true
  mode <- lookupEnv "MODE" >>= case _ of
    Just "local" -> pure Local
    Just "testnet" -> do
      -- TODO add this back when we have an API key again
      -- topup "addr_test1qqevfdhu80jsmjhzkf8lkkv5rza9h6k0u6hmwwr0r7vyjt9j3f374a2all0hc6vzxa6v7sax7du2lk5fu5q592d5fhqswar4hc"
      pure Testnet
    Just e -> throw $ "expected local or testnet got: " <> e
    Nothing -> throw "expected MODE to be set"
  launchAff_ do
    envRunner <- getEnvRunner mode
    runSpec' defaultConfig { timeout = Nothing } [ consoleReporter ] $
      case mode of
        Local -> do
          if runVolumeTests then Test.Volume.HelloWorld.Api.spec
          else do
            describe "pure tests" do
              Test.HelloWorld.Discovery.Api.spec envRunner
              Test.HelloWorld.Discovery.Api.localOnlySpec
              Test.HelloWorld.Signing.spec envRunner
              Test.HelloWorld.Api.spec envRunner
              Test.HelloWorld.Api.localOnlySpec
              Encoding.spec
        Testnet -> do
          describe "pure tests" do
            Encoding.spec
          describe "impure tests" do
            Test.HelloWorld.Signing.spec envRunner
            Test.HelloWorld.Discovery.Api.spec envRunner
            Test.HelloWorld.Api.spec envRunner
