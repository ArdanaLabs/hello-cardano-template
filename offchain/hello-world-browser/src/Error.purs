module HelloWorld.Error where

import Contract.Prelude

import Effect.Exception (Error)

data HelloWorldBrowserError
  = TimeoutError
  | InsufficientFunds
  | OtherError Error
  | NetworkChanged

instance showHelloWorldBrowserError :: Show HelloWorldBrowserError where
  show TimeoutError = "Network timeout occurred. Please refresh the browser."
  show InsufficientFunds = "Insufficient funds. Please topup first."
  show NetworkChanged = "Network changed, press Resume button if you want to see if you have funds that you can redeem on the new network."
  show (OtherError err) = show err
