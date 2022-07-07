module RunCmd(CliState(..),start) where

import Contract.Prelude

import Contract.Monad (Contract, liftContractAffM, logInfo')
import Contract.Scripts (Validator, ValidatorHash,validatorHash)
import Contract.Transaction ( TransactionInput)

import Api(helloScript,sendDatumToScript)

data CliState
  = New
  | Live
  { validator :: Validator
  , validatorHash :: ValidatorHash
  , lastOutput :: TransactionInput
  }

type CliStep = CliState -> Contract () CliState

start :: Int -> Int -> CliStep
start _ _ (Live s) = do
  logInfo' "can't start while live"
  pure $ Live s
start param init New = do
  validator <- helloScript param
  vhash <- liftContractAffM "Couldn't hash validator" $ validatorHash validator
  txid <- sendDatumToScript init vhash
  pure $ Live {validator:validator,validatorHash:vhash,lastOutput:txid}

