module Models.Vault () where

import Apropos

import Plutus.V1.Ledger.Api
import Data.Maybe

import Gen

data VaultModel = VaultModel { balance :: Value , vault :: TxOut }
  deriving stock (Eq,Show)

data VaultProp
  = Matches
  | ValidVault
  deriving stock (Eq,Ord,Enum,Show,Bounded)
  --deriving anyclass Enumerable

-- TODO switch this for anyclass once pr goes through
instance Enumerable VaultProp where
  enumerated = [minBound..maxBound]

instance LogicalModel VaultProp where
  logic = Var Matches :->: Var ValidVault

instance HasLogicalModel VaultProp VaultModel where
  satisfiesProperty Matches VaultModel{vault=vault,balance=balance}
    = txOutValue vault == balance

  satisfiesProperty ValidVault VaultModel{vault=vault}
    = isNothing $ txOutDatumHash vault

instance HasParameterisedGenerator VaultProp VaultModel where
  parameterisedGenerator s =
    let var p = p `elem` s
    in
      if var ValidVault
         then do
           adr <- address
           val1 <- value
           val2 <- value
           pure $ VaultModel val1 (TxOut adr val2 Nothing)
         else if var Matches
           then do
           adr <- address
           val <- value
           pure $ VaultModel val (TxOut adr val Nothing)
           else do
             adr <- address
             val1 <- value
             val2 <- value
             mdh <- maybeOf datumHash
             pure $ VaultModel val1 (TxOut adr val2 mdh)

