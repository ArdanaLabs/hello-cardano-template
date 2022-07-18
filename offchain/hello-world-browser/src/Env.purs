module Env where

import Contract.Monad (DefaultContractConfig)

type Env =
  { contractConfig :: DefaultContractConfig
  }