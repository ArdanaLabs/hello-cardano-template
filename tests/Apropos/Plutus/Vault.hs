module Apropos.Plutus.Vault (spec) where

import Apropos

import Gen

import Plutus.V1.Ledger.Api (
    Datum(..),
    DatumHash,
    TxOut,
    toBuiltinData,
 )

import Apropos.Plutus.AssetClass (ada,dusd)
import Plutus.V1.Ledger.Value (
    Value,
 )

import Test.Syd (Spec, xdescribe)
import Test.Syd.Hedgehog (fromHedgehogGroup)
import Apropos.Plutus.SingletonValue (SingletonValue)

spec :: Spec
spec = do
    -- TODO figure out value retry problem
    -- and enable this test
    xdescribe "vault model" $ do
        fromHedgehogGroup $ runGeneratorTestsWhere (Apropos :: VaultModel :+ VaultProp) "generator" Yes

data VaultModel = VaultModel
  { colatoral :: SingletonValue
  , debt :: SingletonValue
  , vault :: TxOut
  , datumEntry :: (Datum, DatumHash)
  , balance :: Value
  }
    deriving stock (Eq, Show)

makeVaultDatum :: SingletonValue -> SingletonValue -> Datum
makeVaultDatum colatoral debt = Datum $ toBuiltinData (colatoral,debt)

-- TODO real hashes
hashDatum :: Datum -> DatumHash
hashDatum _ = "a"

data VaultProp
    = HasCorrectDatum
    deriving stock (Eq, Ord, Enum, Show, Bounded)

--deriving anyclass Enumerable

-- TODO switch this for anyclass once pr goes through
instance Enumerable VaultProp where
    enumerated = [minBound .. maxBound]

instance LogicalModel VaultProp where
    logic = Yes

instance HasLogicalModel VaultProp VaultModel where
  satisfiesProperty HasCorrectDatum vm = fst (datumEntry vm) == makeVaultDatum (colatoral vm) (debt vm)

instance HasPermutationGenerator VaultProp VaultModel where
  generators =
    [ Morphism
      { name = "Fix Datum"
      , match = Not $ Var HasCorrectDatum
      , contract = add HasCorrectDatum
      , morphism = \vm -> do
        let vaultDatum = makeVaultDatum (colatoral vm) (debt vm)
        pure $ vm {datumEntry = (vaultDatum,hashDatum vaultDatum)}
      }
    ]

instance HasParameterisedGenerator VaultProp VaultModel where
    parameterisedGenerator = buildGen baseGen

baseGen :: Gen VaultModel
baseGen = do
  let colatoral = (ada,0)
      debt = (dusd,0)
      vaultDatum = makeVaultDatum colatoral debt
  -- TODO add datum enrty to txout
  vault <- txOut
  return $ VaultModel
    { colatoral = colatoral
    , debt = debt
    , balance = mempty
    , vault = vault
    , datumEntry = (vaultDatum,hashDatum vaultDatum)
    }
