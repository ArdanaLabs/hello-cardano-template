module Apropos.Plutus.Vault (spec) where

import Apropos
import Gen

import Apropos.Plutus.AssetClass (ada, dusd)
import Apropos.Plutus.SingletonValue (SingletonValue)
import Control.Monad (when)
import Data.Ratio
import GHC.Generics (Generic)
import Plutus.V1.Ledger.Api (
    Datum (..),
    DatumHash,
    TxOut (..),
    toBuiltinData,
 )
import Plutus.V1.Ledger.Value (Value, assetClassValue)

import Test.Syd
import Test.Syd.Hedgehog (fromHedgehogGroup)

spec :: Spec
spec = do
    describe "vault model" $ do
        fromHedgehogGroup $ runGeneratorTestsWhere (Apropos :: VaultModel :+ VaultProp) "generator" Yes

data VaultModel = VaultModel
    { colatoral :: SingletonValue
    , debt :: SingletonValue
    , vault :: TxOut
    , datumEntry :: (Datum, DatumHash)
    , balance :: Value
    }
    deriving stock (Eq, Show)

liquidityRatio :: Rational
liquidityRatio = 3 % 2

makeVaultDatum :: SingletonValue -> SingletonValue -> Datum
makeVaultDatum colatoral debt = Datum $ toBuiltinData (colatoral, debt)

-- TODO real hashes
hashDatum :: Datum -> DatumHash
hashDatum _ = "aa"

data VaultProp
    = HasCorrectDatum
    | CanLiquidate
    deriving stock (Eq, Ord, Enum, Show, Bounded, Generic)
    deriving anyclass (Enumerable)

instance LogicalModel VaultProp where
    logic = Yes

instance HasLogicalModel VaultProp VaultModel where
    satisfiesProperty HasCorrectDatum vm = fst (datumEntry vm) == makeVaultDatum (colatoral vm) (debt vm)
    satisfiesProperty CanLiquidate vm = fromIntegral (snd (colatoral vm)) < liquidityRatio * fromIntegral (snd (debt vm))

instance HasPermutationGenerator VaultProp VaultModel where
    generators =
        [ Morphism
            { name = "Fix Datum"
            , match = Not $ Var HasCorrectDatum
            , contract = add HasCorrectDatum
            , morphism = \vm -> do
                let vaultDatum = makeVaultDatum (colatoral vm) (debt vm)
                let vaultDatumHash = hashDatum vaultDatum
                pure $ vm{datumEntry = (vaultDatum, vaultDatumHash), vault = (vault vm){txOutDatumHash = Just vaultDatumHash}}
            }
        , Morphism
            { name = "Break Datum"
            , match = Var HasCorrectDatum
            , contract = remove HasCorrectDatum
            , morphism = \vm -> do
                let vaultDatum = makeVaultDatum (colatoral vm) (debt vm)
                wrongDatum <- genFilter (/= vaultDatum) datum
                let wrongDatumHash = hashDatum wrongDatum
                pure $ vm{datumEntry = (wrongDatum, wrongDatumHash), vault = (vault vm){txOutDatumHash = Just wrongDatumHash}}
            }
        , Morphism
            { name = "increase debt"
            , match = Not $ Var CanLiquidate
            , contract = add CanLiquidate >> remove HasCorrectDatum
            , morphism = \vm -> do
                -- TODO do these morphisms need to use fromIntegral?
                let minDebt = fromIntegral $ 3 * snd (colatoral vm) `div` 2 + 1
                debt' <- int (linear minDebt (max 1_000_00 (2 * minDebt)))
                let vm' = vm{debt = (fst $ debt vm, fromIntegral debt')}
                when (satisfiesProperty HasCorrectDatum vm') retry
                pure vm'
            }
        , Morphism
            { name = "reduce debt"
            , match = Var CanLiquidate
            , contract = remove CanLiquidate >> remove HasCorrectDatum
            , morphism = \vm -> do
                let maxDebt = fromIntegral $ 3 * snd (colatoral vm) `div` 2
                debt' <- int (linear 0 maxDebt)
                let vm' = vm{debt = (fst $ debt vm, fromIntegral debt')}
                when (satisfiesProperty HasCorrectDatum vm') retry
                pure vm'
            }
        ]

instance HasParameterisedGenerator VaultProp VaultModel where
    parameterisedGenerator = buildGen baseGen

baseGen :: Gen VaultModel
baseGen = do
    let colatoral = (ada, 0)
        debt = (dusd, 0)
        vaultDatum = makeVaultDatum colatoral debt
    adr <- address
    let val = assetClassValue ada 0
    let vault = TxOut adr val (Just $ hashDatum vaultDatum)
    return $
        VaultModel
            { colatoral = colatoral
            , debt = debt
            , balance = mempty
            , vault = vault
            , datumEntry = (vaultDatum, hashDatum vaultDatum)
            }
