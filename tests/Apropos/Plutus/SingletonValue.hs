-- {-# LANGUAGE TemplateHaskell #-}

module Apropos.Plutus.SingletonValue (
  --singletonValueGenSelfTests,
  SingletonValue,
  SingletonValueProp,
  ) where
--import Apropos
--import Test.Tasty (TestTree, testGroup)
--import Test.Tasty.Hedgehog (fromGroup)
import Plutus.V1.Ledger.Value (AssetClass)
import Apropos.Plutus.AssetClass (AssetClassProp)
import Apropos.Plutus.Integer (IntegerProp)
--import Control.Monad(join)

type SingletonValue = (AssetClass,Integer)

data SingletonValueProp
  = AC AssetClassProp
  | Amt IntegerProp
  deriving stock (Eq, Ord, Show)

-- $(genEnumerable ''SingletonValueProp)



  {-
singletonValueGenSelfTests :: TestTree
singletonValueGenSelfTests =
  testGroup "singletonValueGenSelfTests" $
    fromGroup
      <$> permutationGeneratorSelfTest
        True
        (\(_ :: Morphism SingletonValueProp singletonValueGenSelfTests) -> True)
        baseGen

-}
