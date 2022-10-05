module Test.Main
  ( main
  ) where

import Prelude

import Data.Maybe (Maybe(Just, Nothing))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Exception (throw)
import Faucet (topup)
import Node.Process (lookupEnv)
import Test.HelloWorld.EnvRunner (Mode(..), getEnvRunner)
import Test.Spec (describe)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec', defaultConfig)

import Test.HelloWorld.Encoding as Encoding
import Test.HelloWorld.Api as Test.HelloWorld.Api
import Test.HelloWorld.Discovery.Api as Test.HelloWorld.Discovery.Api

main :: Effect Unit
main = do
  mode <- lookupEnv "MODE" >>= case _ of
    Just "local" -> pure Local
    Just "testnet" -> do
      -- topup "addr_test1qqevfdhu80jsmjhzkf8lkkv5rza9h6k0u6hmwwr0r7vyjt9j3f374a2all0hc6vzxa6v7sax7du2lk5fu5q592d5fhqswar4hc"
      pure Testnet
    Just e -> throw $ "expected local or testnet got: " <> e
    Nothing -> throw "expected MODE to be set"
  launchAff_ do
    envRunner <- getEnvRunner mode
    runSpec' defaultConfig { timeout = Nothing } [ consoleReporter ] $
      case mode of
        Local -> do
          describe "pure tests" do
            Test.HelloWorld.Discovery.Api.spec envRunner
            Test.HelloWorld.Discovery.Api.localOnlySpec
            Test.HelloWorld.Api.spec envRunner
            Test.HelloWorld.Api.localOnlySpec
            Encoding.spec
        Testnet -> do
          describe "pure tests" do
            Encoding.spec
          describe "impure tests" do
            Test.HelloWorld.Discovery.Api.spec envRunner
            Test.HelloWorld.Api.spec envRunner
