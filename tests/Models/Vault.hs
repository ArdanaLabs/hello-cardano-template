module Models.Vault (spec,hedge) where

import Apropos
    ( retry,
      runGeneratorTestsWhere,
      choice,
      genFilter,
      HasLogicalModel(satisfiesProperty),
      HasParameterisedGenerator(parameterisedGenerator),
      LogicalModel(..),
      Enumerable(..),
      Formula(Yes, Var, (:->:)),
      type (:+),
      Apropos(Apropos) ,
    )

import Gen ( address, assetClass, datum, datumHash, value )

import Plutus.V1.Ledger.Api
    ( TxOut(TxOut, txOutValue, txOutDatumHash),
      Datum,
      DatumHash,
    )

import Plutus.V1.Ledger.Value (
  AssetClass,
  Value,
  )

import Test.Syd ( xdescribe, Spec )
import Test.Syd.Hedgehog (fromHedgehogGroup)
import Control.Monad ( when )
import Hedgehog (Group)

import Debug.Trace (traceShow)

spec :: Spec
spec = do
  -- TODO figure out value retry problem
  -- and enable this test
  xdescribe "vault model" $ do
    fromHedgehogGroup $ runGeneratorTestsWhere (Apropos :: VaultModel :+ VaultProp) "generator" Yes

hedge :: Group
hedge = runGeneratorTestsWhere (Apropos :: VaultModel :+ VaultProp) "generator" Yes


data VaultModel = VaultModel { balance :: Value , vault :: TxOut, asset :: AssetClass , datumEntry :: (Datum,DatumHash) }
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

  satisfiesProperty ValidVault VaultModel{vault=vault,datumEntry=datumEntry}
    = txOutDatumHash vault == Just (snd datumEntry)

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
                        when (val1 == val2) $ do
                          traceShow (val1,val2) $ return ()
                          retry
                        return (val1,val2)
    asset <- assetClass
    d <- datum
    dh <- datumHash
    mdh <- if var ValidVault
              then pure $ Just dh
              else choice [ pure Nothing , Just <$> genFilter (/= dh) datumHash ]
    pure $ VaultModel val1 (TxOut adr val2 mdh) asset (d,dh)
