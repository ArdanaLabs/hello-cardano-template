module HelloWorld.Error where

import Contract.Prelude

import Effect.Exception (Error)

data HelloWorldBrowserError
  = TimeoutError
  | InsufficientFunds
  | OtherError Error

instance showHelloWorldBrowserError :: Show HelloWorldBrowserError where
  show TimeoutError = "Network timeout occurred. Please refresh the browser."
  show InsufficientFunds = "Insufficient funds. Please topup first."
  show (OtherError err) = show err
