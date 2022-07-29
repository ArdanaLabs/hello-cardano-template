module HelloWorld.Error where

import Effect.Exception (Error)

timeoutErrorMessage :: String
timeoutErrorMessage = "Network timeout occurred. Please refresh the browser."

data HelloWorldBrowserError
  = TimeoutError
  | OtherError Error

