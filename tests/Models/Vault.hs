module Models.Vault (spec) where

import Apropos

import Gen

import Plutus.V1.Ledger.Api
import Data.Maybe

import Test.Syd
import Test.Syd.Hedgehog (fromHedgehogGroup)
import Control.Monad

import Debug.Trace

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
  parameterisedGenerator s = do
    let var p = p `elem` s
    adr <- address
    (val1,val2) <- if var Matches
                      then do
                        val <- value
                        return (val,val)
                      else do
                        val1 <- value
                        val2 <-  value
                        traceShow (val1,val2) $ return ()
                        when (val1 == val2) retry
                        return (val1,val2)
    mdh <- if var ValidVault
               then pure Nothing
               else Just <$> datumHash
    pure $ VaultModel val1 (TxOut adr val2 mdh)
