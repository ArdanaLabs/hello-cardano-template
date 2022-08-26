module Apropos.ContextBuilder (
  TxInfoBuilder (..),
  ScriptContextBuilder (..),
  buildContext,
  withTxInfo,
  nullTxId,
  nullTxOutRef,
  applyValidator,
  applyMintingPolicy,
) where

import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.State (StateT, execStateT, get, modify)
import Data.Functor.Identity (Identity, runIdentity)
import PlutusLedgerApi.V1.Interval (Extended (..), Interval (..), LowerBound (..), UpperBound (..))
import PlutusLedgerApi.V1.Scripts (applyArguments)
import PlutusLedgerApi.V2 (
  Address,
  BuiltinByteString,
  DCert,
  OutputDatum (NoOutputDatum, OutputDatum),
  POSIXTimeRange,
  PubKeyHash,
  ScriptContext (..),
  ScriptPurpose (..),
  TxId (..),
  TxInInfo (..),
  TxInfo (..),
  TxOut (..),
  TxOutRef (..),
  Value (..),
  Validator(..),
  MintingPolicy(..),
  Redeemer,
  Script,
  Datum,
  fromList,
  toData,
 )

-- with concrete types and extra packaging for convenience
buildContext :: StateT ScriptContext Identity () -> ScriptContext
buildContext builder = runIdentity $ buildScriptContext @(StateT ScriptContext) @Identity builder

-- with concrete types for convenience
withTxInfo :: StateT TxInfo Identity () -> StateT ScriptContext Identity ()
withTxInfo = withTxInfoBuilder

nullTxId :: TxId
nullTxId = TxId "0000000000000000000000000000000000000000000000000000000000000000"

nullTxOutRef :: TxOutRef
nullTxOutRef = TxOutRef nullTxId 0

emptyScriptContext :: ScriptContext
emptyScriptContext = ScriptContext emptyTxInfo (Spending (TxOutRef nullTxId 0))

class (MonadTrans t, Monad m, Monad (t m)) => ScriptContextBuilder t m where
  runScriptContextBuilder :: ScriptContext -> t m () -> m ScriptContext
  buildScriptContext :: t m () -> m ScriptContext
  getTxInfo :: t m TxInfo
  setTxInfo :: TxInfo -> t m ()
  setScriptPurpose :: ScriptPurpose -> t m ()
  withTxInfoBuilder :: TxInfoBuilder u m => u m () -> t m ()
  withTxInfoBuilder b = getTxInfo >>= (\t -> lift (runTxInfoBuilder t b)) >>= setTxInfo

instance Monad m => ScriptContextBuilder (StateT ScriptContext) m where
  runScriptContextBuilder = flip execStateT
  buildScriptContext = runScriptContextBuilder emptyScriptContext
  getTxInfo = scriptContextTxInfo <$> get
  setTxInfo t = modify (\s -> s {scriptContextTxInfo = t})
  setScriptPurpose s = modify (\sc -> sc {scriptContextPurpose = s})

emptyTxInfo :: TxInfo
emptyTxInfo =
  TxInfo
    { txInfoInputs = []
    , txInfoReferenceInputs = []
    , txInfoOutputs = []
    , txInfoFee = Value (fromList [])
    , txInfoMint = Value (fromList [])
    , txInfoDCert = []
    , txInfoWdrl = fromList []
    , txInfoValidRange = Interval (LowerBound NegInf True) (UpperBound PosInf True)
    , txInfoSignatories = []
    , txInfoData = fromList []
    , txInfoId = nullTxId
    , txInfoRedeemers = fromList []
    }

class (MonadTrans t, Monad m) => TxInfoBuilder t m where
  runTxInfoBuilder :: TxInfo -> t m () -> m TxInfo
  buildTxInfo :: t m () -> m TxInfo
  addInput :: TxOutRef -> Address -> Value -> Maybe Datum -> t m ()
  addOutput :: Address -> Value -> Maybe Datum -> t m ()

  addFee :: Value -> t m ()
  mint :: Value -> t m ()

  addTxInfoInput :: TxInInfo -> t m ()
  addTxInfoOutput :: TxOut -> t m ()
  addTxInfoDCert :: DCert -> t m ()

  --addTxInfoWdrl :: (StakingCredential, Integer) -> t m ()
  addTxInfoSignatory :: PubKeyHash -> t m ()

  --addTxInfoData :: (DatumHash, Datum) -> t m ()

  setTxInfoInputs :: [TxInInfo] -> t m ()
  setTxInfoOutputs :: [TxOut] -> t m ()
  setTxInfoFee :: Value -> t m ()
  setTxInfoMint :: Value -> t m ()
  setTxInfoDCert :: [DCert] -> t m ()

  --setTxInfoWdrl :: [(StakingCredential, Integer)] -> t m ()
  setTxInfoValidRange :: POSIXTimeRange -> t m ()
  setTxInfoSignatories :: [PubKeyHash] -> t m ()

  --setTxInfoData :: [(DatumHash, Datum)] -> t m ()
  setTxInfoId :: BuiltinByteString -> t m ()

instance Monad m => TxInfoBuilder (StateT TxInfo) m where
  runTxInfoBuilder = flip execStateT
  buildTxInfo = runTxInfoBuilder emptyTxInfo
  addInput r a v d =
    let i = TxInInfo r (TxOut a v (maybe NoOutputDatum OutputDatum d) Nothing) -- TODO what's with the maybe script hash now?
     in modify
          ( \txi ->
              txi
                { txInfoInputs = txInfoInputs txi <> [i]
                }
          )
  addOutput a v d =
    let i = TxOut a v (maybe NoOutputDatum OutputDatum d) Nothing -- TODO here too
     in modify
          ( \txi ->
              txi
                { txInfoOutputs = txInfoOutputs txi <> [i]
                }
          )

  addFee f = modify (\txi -> txi {txInfoFee = f <> txInfoFee txi})
  mint f = modify (\txi -> txi {txInfoMint = f <> txInfoMint txi})
  addTxInfoInput i = modify (\txi -> txi {txInfoInputs = txInfoInputs txi <> [i]})
  addTxInfoOutput o = modify (\txi -> txi {txInfoOutputs = txInfoOutputs txi <> [o]})
  addTxInfoDCert d = modify (\txi -> txi {txInfoDCert = txInfoDCert txi <> [d]})

  --addTxInfoWdrl w = modify (\txi -> txi {txInfoWdrl = txInfoWdrl txi <> fromList [w]})
  addTxInfoSignatory s = modify (\txi -> txi {txInfoSignatories = txInfoSignatories txi <> [s]})

  --addTxInfoData d = modify (\txi -> txi {txInfoData = txInfoData txi <> [d]})

  setTxInfoInputs i = modify (\txi -> txi {txInfoInputs = i})
  setTxInfoOutputs o = modify (\txi -> txi {txInfoOutputs = o})
  setTxInfoFee f = modify (\txi -> txi {txInfoFee = f})
  setTxInfoMint m = modify (\txi -> txi {txInfoMint = m})
  setTxInfoDCert d = modify (\txi -> txi {txInfoDCert = d})

  --setTxInfoWdrl w = modify (\txi -> txi {txInfoWdrl = fromList w})
  setTxInfoValidRange r = modify (\txi -> txi {txInfoValidRange = r})
  setTxInfoSignatories s = modify (\txi -> txi {txInfoSignatories = s})

  --setTxInfoData d = modify (\txi -> txi {txInfoData = fromList d})
  setTxInfoId b = modify (\txi -> txi {txInfoId = TxId b})


-- For some reason plutus doesn't have this for V2 so I wrote it here
applyValidator :: ScriptContext -> Validator -> Datum -> Redeemer -> Script
applyValidator sc (Validator s) d r = applyArguments s [toData d, toData r, toData sc]

applyMintingPolicy :: ScriptContext -> MintingPolicy -> Redeemer -> Script
applyMintingPolicy sc (MintingPolicy s) r = applyArguments s [toData r, toData sc]
