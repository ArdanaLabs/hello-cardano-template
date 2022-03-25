module Apropos.Plutus.Vault (
  spec,
  VaultProp,
  makeVaultDatum,
                            ) where

import Apropos

import Apropos.Plutus.AssetClass (AssetClassProp(..))
import Apropos.Plutus.SingletonValue (SingletonValue,SingletonValueProp(..))
import Apropos.Plutus.Integer (IntegerProp(..))
import GHC.Generics (Generic)
import Plutus.V1.Ledger.Api (
    Datum (..),
 )
import Plutus.V1.Ledger.Value (AssetClass(..))
import Control.Lens (lens)

import Test.Syd
import Test.Syd.Hedgehog (fromHedgehogGroup)

import Plutarch ( (#), PCon(pcon) )
import Plutarch.Lift ( pconstant, plift )
import Plutarch.Builtin
    ( PIsData(pdata), pforgetData, ppairDataBuiltin )
import Plutarch.Api.V1 ( PDatum(PDatum) )

spec :: Spec
spec = do
    describe "vault model" $ do
        fromHedgehogGroup $ runGeneratorTestsWhere (Apropos :: VaultModel :+ VaultProp) "generator" Yes

data VaultModel = VaultModel
    { collateral :: SingletonValue
    , debt :: SingletonValue
    -- TODO add address model
    }
    deriving stock (Eq, Show)

data VaultProp
    = DebtProp SingletonValueProp
    | CollateralProp SingletonValueProp
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (Enumerable)

instance LogicalModel VaultProp where
    logic = abstractionLogic @VaultModel
      -- logic for performance
      :&&: Var (CollateralProp (AC IsAda))
      :&&: Var (DebtProp (AC IsDUSD))
      :&&: Not (Var (CollateralProp (Amt IsNegative)))
      :&&: Not (Var (DebtProp (Amt IsNegative)))

instance HasLogicalModel VaultProp VaultModel where
    satisfiesProperty (DebtProp p) vm = satisfiesProperty p (debt vm)
    satisfiesProperty (CollateralProp p) vm = satisfiesProperty p (collateral vm)

instance HasAbstractions VaultProp VaultModel where
  abstractions =
    [ WrapAbs $
      ProductAbstraction
        { abstractionName = "debt"
        , propertyAbstraction = abstractsProperties DebtProp
        , productModelAbstraction = lens debt (\vm debt' -> vm{debt=debt'})
        }
    , WrapAbs $
      ProductAbstraction
        { abstractionName = "collateral"
        , propertyAbstraction = abstractsProperties CollateralProp
        , productModelAbstraction = lens collateral (\vm collateral' -> vm{collateral=collateral'})
        }
    ]

instance HasPermutationGenerator VaultProp VaultModel where
  generators = abstractionMorphisms

instance HasParameterisedGenerator VaultProp VaultModel where
    parameterisedGenerator = buildGen baseGen

baseGen :: Gen VaultModel
baseGen = VaultModel
  <$> genSatisfying (Not (Var (Amt IsNegative)) :&&: Var (AC IsAda))
  <*> genSatisfying (Not (Var (Amt IsNegative)) :&&: Var (AC IsDUSD))

makeVaultDatum :: VaultModel -> Datum
makeVaultDatum VaultModel{collateral=(AssetClass (collateralCs,collateralTn),collateralAmt),debt=(AssetClass (debtCs,debtTn),debtAmt)} =
    -- TODO we may want to change the way the vault datum is encoded
    plift $ pcon $ PDatum $ pforgetData $ pdata $
      ppairDataBuiltin
        # pdata ( ppairDataBuiltin
            # pdata (ppairDataBuiltin # pdata (pconstant collateralCs) # pdata (pconstant collateralTn))
            # pdata (pconstant collateralAmt)
          )
        # pdata ( ppairDataBuiltin
            # pdata (ppairDataBuiltin # pdata (pconstant debtCs) # pdata (pconstant debtTn))
            # pdata (pconstant debtAmt)
          )

-- TODO makeVaultTxout
