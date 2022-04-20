-- | Provides the hello world contract
module HelloWorld.Contract (
  Schema,
  contract,
) where

import Control.Monad (forever)
import Data.Text (Text)
import Plutus.Contract (
  Contract,
  Endpoint,
  type (.\/),
 )
import Plutus.Contract qualified as PContract

-- | REST schema
type Schema =
  Endpoint "initialize" ()
    .\/ Endpoint "increment" ()
    .\/ Endpoint "read" ()

initialize :: () -> Contract w s e ()
initialize _ = pure ()

increment :: () -> Contract w s e ()
increment _ = pure ()

readContract :: () -> Contract w s e ()
readContract _ = pure ()

-- | The contract definition
contract :: Contract () Schema Text ()
contract =
  forever $
    PContract.selectList
      [ PContract.endpoint @"initialize" initialize
      , PContract.endpoint @"increment" increment
      , PContract.endpoint @"read" readContract
      ]
