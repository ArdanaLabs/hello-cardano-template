module Test.Main
  ( main
  ) where

import Prelude

import Data.Maybe (Maybe(Just, Nothing))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Exception (throw)
import Node.Process (lookupEnv)
import Test.Encoding as Encoding
import Test.HelloWorld.Api as Test.HelloWorld.Api
import Test.Spec (Spec, describe)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec', defaultConfig)

data Mode = Local | Testnet

main :: Effect Unit
main = do
  mode <- lookupEnv "MODE" >>= case _ of
    Just "local" -> pure Local
    Just "testnet" -> pure Testnet
    Just e -> throw $ "expected local or testnet got: " <> e
    Nothing -> throw "expected MODE to be set"
  launchAff_ $ runSpec' defaultConfig { timeout = Nothing } [ consoleReporter ] $
    case mode of
      Local -> do
        describe "pure tests" do
          Test.HelloWorld.Api.localOnlySpec
          spec
      Testnet -> do
        describe "impure tests" do
          spec

spec :: Spec Unit
spec = do
  Encoding.spec
  Test.HelloWorld.Api.spec
