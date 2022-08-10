module Apropos.Plutus.Vault (
  spec,
  VaultProp,
  makeVaultDatum,
) where

import Apropos

import Apropos.Plutus.AssetClass (AssetClassProp (..))
import Apropos.Plutus.Integer (IntegerProp (..))
import Apropos.Plutus.SingletonValue (SingletonValue, SingletonValueProp (..))

{- HLINT ignore Avoid restricted module -}
import Control.Lens (lens)
import Plutus.V1.Ledger.Api (
  Datum (..),
 )
import Plutus.V1.Ledger.Value (AssetClass (..))

import Test.Syd
import Test.Syd.Hedgehog (fromHedgehogGroup)

import Plutarch (PCon (pcon), (#))
import Plutarch.Api.V1 (PDatum (PDatum))
import Plutarch.Builtin (
  pforgetData,
  ppairDataBuiltin,
 )
import Plutarch.Lift (pconstant, plift)
import Plutarch.Prelude (pdata)

spec :: Spec
spec = do
  describe "vault model" $ do
    fromHedgehogGroup $ runGeneratorTestsWhere @VaultProp "generator" Yes

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
  deriving anyclass (Enumerable, Hashable)

instance LogicalModel VaultProp where
  logic =
    abstractionLogic @VaultModel
      -- logic for performance
      :&&: Var (CollateralProp (AC IsAda))
      :&&: Var (DebtProp (AC IsDUSD))
      :&&: Not (Var (CollateralProp (Amt IsNegative)))
      :&&: Not (Var (DebtProp (Amt IsNegative)))

instance HasLogicalModel VaultProp VaultModel where
  satisfiesProperty (DebtProp p) vm = satisfiesProperty p (debt vm)
  satisfiesProperty (CollateralProp p) vm = satisfiesProperty p (collateral vm)

instance HasAbstractions VaultProp VaultModel where
  sourceAbstractions =
    [ SoAs $
        SourceAbstraction
          { sourceAbsName = "VaultModel"
          , constructor = VaultModel
          , productAbs =
              ProductAbstraction
                { abstractionName = "collateral"
                , propertyAbstraction = abstractsProperties CollateralProp
                , productModelAbstraction = lens collateral (\vm collateral' -> vm {collateral = collateral'})
                }
                :& ProductAbstraction
                  { abstractionName = "debt"
                  , propertyAbstraction = abstractsProperties DebtProp
                  , productModelAbstraction = lens debt (\vm debt' -> vm {debt = debt'})
                  }
                :& Nil
          }
    ]

instance HasPermutationGenerator VaultProp VaultModel where
  sources = abstractionSources

instance HasParameterisedGenerator VaultProp VaultModel where
  parameterisedGenerator = buildGen

makeVaultDatum :: VaultModel -> Datum
makeVaultDatum VaultModel {collateral = (AssetClass (collateralCs, collateralTn), collateralAmt), debt = (AssetClass (debtCs, debtTn), debtAmt)} =
  -- TODO we may want to change the way the vault datum is encoded
  plift $
    pcon $
      PDatum $
        pforgetData $
          pdata $
            ppairDataBuiltin
              # pdata
                ( ppairDataBuiltin
                    # pdata (ppairDataBuiltin # pdata (pconstant collateralCs) # pdata (pconstant collateralTn))
                    # pdata (pconstant collateralAmt)
                )
              # pdata
                ( ppairDataBuiltin
                    # pdata (ppairDataBuiltin # pdata (pconstant debtCs) # pdata (pconstant debtTn))
                    # pdata (pconstant debtAmt)
                )

-- TODO makeVaultTxout
