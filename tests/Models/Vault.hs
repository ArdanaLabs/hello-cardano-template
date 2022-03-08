module Models.Vault (spec) where

import Apropos

import Gen

import Plutus.V1.Ledger.Api
import Data.Maybe

import Test.Syd
import Test.Syd.Hedgehog (fromHedgehogGroup)

spec :: Spec
spec = do
  describe "vault model" $ do
    fromHedgehogGroup $ runGeneratorTestsWhere (Apropos :: VaultModel :+ VaultProp) "" Yes


data VaultModel = VaultModel { balance :: Value , vault :: TxOut }
  deriving stock (Eq,Show)

data VaultProp
  = Matches
  | ValidVault -- TODO figure out what valid vault datum should actually be
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
      if var Matches
         then do
           adr <- address
           val <- value
           pure $ VaultModel val (TxOut adr val Nothing)
         else if var ValidVault
           then do
           adr <- address
           val1 <- value
           val2 <- genFilter (/= val1) value
           pure $ VaultModel val1 (TxOut adr val2 Nothing)
           else do
             adr <- address
             val1 <- value
             val2 <- genFilter (/= val1) value
             mdh <- Just <$> datumHash
             pure $ VaultModel val1 (TxOut adr val2 mdh)
