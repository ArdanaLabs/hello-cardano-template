module Test.HelloWorld.Discovery.Api
  ( spec
  ) where

import Contract.Prelude

import Contract.Address (getWalletAddress)
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftContractAffM, liftContractM)
import Contract.ScriptLookups as Lookups
import Contract.Test.Plutip (runContractInEnv)
import Contract.TxConstraints (TxConstraints)
import Contract.TxConstraints as Constraints
import Contract.Utxos (getUtxo, getWalletBalance)
import Contract.Value (adaToken, scriptCurrencySymbol)
import Contract.Wallet (withKeyWallet)
import Data.BigInt as BigInt
import Data.Time.Duration (Minutes(..))
import Effect.Exception (throw)
import HelloWorld.Discovery.Api (makeNftPolicy, mintNft, seedTx)
import Plutus.Types.Value as Value
import Test.HelloWorld.EnvRunner (EnvRunner)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (expectError, shouldEqual)
import Types.PlutusData (PlutusData)
import Util (buildBalanceSignAndSubmitTx, waitForTx, withOurLogger,maxWait)

spec :: EnvRunner -> Spec Unit
spec runner = do
  describe "HelloWorld.Discovery.Api" $ do
    describe "nft" do
      traverse_ (_ $ runner)
        [ tryMintNft
        , tryDoubleMint
        , txSpentAfterMint
        , mintingWorks
        , cantBurn
        ]

useRunnerSimple :: forall a. String -> Contract () a -> EnvRunner -> Spec Unit
useRunnerSimple name contract runner = do
  it name $ runner \env alice -> do
    runContractInEnv (withOurLogger "apiTest.log" env)
      $ withKeyWallet alice
      $ void contract

tryMintNft :: EnvRunner -> Spec Unit
tryMintNft = useRunnerSimple "mint runs" $ mintNft

tryDoubleMint :: EnvRunner -> Spec Unit
tryDoubleMint =
  useRunnerSimple "double minting fails on second mint" $ do
    txOut <- seedTx
    nftPolicy <- makeNftPolicy txOut
    cs <- liftContractAffM "hash failed" $ scriptCurrencySymbol nftPolicy
    let
      lookups :: Lookups.ScriptLookups PlutusData
      lookups = Lookups.mintingPolicy nftPolicy

      constraints :: TxConstraints Unit Unit
      constraints =
        Constraints.mustSpendPubKeyOutput txOut
        <> Constraints.mustMintValue (Value.singleton cs adaToken (BigInt.fromInt 1))
    txId <- buildBalanceSignAndSubmitTx lookups constraints
    adr <- liftContractM "no wallet" =<< getWalletAddress
    _ <- waitForTx maxWait adr txId
    expectError $ buildBalanceSignAndSubmitTx lookups constraints
    pure unit

txSpentAfterMint :: EnvRunner -> Spec Unit
txSpentAfterMint = useRunnerSimple "seedTx is spent after mint" $ do
  txOut <- seedTx
  nftPolicy <- makeNftPolicy txOut
  cs <- liftContractAffM "hash failed" $ scriptCurrencySymbol nftPolicy
  logInfo' $ "NFT cs: " <> show cs
  let
    lookups :: Lookups.ScriptLookups PlutusData
    lookups = Lookups.mintingPolicy nftPolicy

    constraints :: TxConstraints Unit Unit
    constraints =
      Constraints.mustSpendPubKeyOutput txOut
      <> Constraints.mustMintValue (Value.singleton cs adaToken (BigInt.fromInt 1))
  txId <- buildBalanceSignAndSubmitTx lookups constraints
  adr <- liftContractM "no Address" =<< getWalletAddress
  _ <- liftContractM "wait timed out" =<< waitForTx waitTime adr txId
  getUtxo txOut >>= case _ of
    Nothing -> pure unit
    Just _ -> liftEffect $ throw "seed tx still existed"

mintingWorks :: EnvRunner -> Spec Unit
mintingWorks = useRunnerSimple "wallet has nft after mint" $ do
  cs <- mintNft
  bal <- liftContractM "no ballance" =<< getWalletBalance
  let nfts = Value.valueOf bal cs adaToken
  nfts `shouldEqual` (BigInt.fromInt 1)

cantBurn :: EnvRunner -> Spec Unit
cantBurn = useRunnerSimple "burning nft fails" $ do
  txOut <- seedTx
  nftPolicy <- makeNftPolicy txOut
  cs <- liftContractAffM "hash failed" $ scriptCurrencySymbol nftPolicy
  logInfo' $ "NFT cs: " <> show cs
  let
    mintLookups :: Lookups.ScriptLookups PlutusData
    mintLookups = Lookups.mintingPolicy nftPolicy

    mintConstraints :: TxConstraints Unit Unit
    mintConstraints =
      Constraints.mustSpendPubKeyOutput txOut
      <> Constraints.mustMintValue (Value.singleton cs adaToken (BigInt.fromInt 1))
  txid <- buildBalanceSignAndSubmitTx mintLookups mintConstraints
  adr <- liftContractM "no wallet" =<< getWalletAddress
  _ <- waitForTx waitTime adr txid
  let
    burnLookups :: Lookups.ScriptLookups PlutusData
    burnLookups = Lookups.mintingPolicy nftPolicy

    burnConstraints :: TxConstraints Unit Unit
    burnConstraints =
      Constraints.mustSpendPubKeyOutput txOut
      <> Constraints.mustMintValue (Value.singleton cs adaToken (BigInt.fromInt $ -1))
  expectError
    $ void
    $ buildBalanceSignAndSubmitTx burnLookups burnConstraints

waitTime :: Minutes
waitTime = 5.0 # Minutes
