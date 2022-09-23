module Apropos.ContextBuilder (
  TxInfoBuilder (..),
  ScriptContextBuilder (..),
  buildContext,
  withTxInfo,
  nullTxId,
  nullTxOutRef,
) where

import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.State (StateT, execStateT, get, modify)
import Data.Functor.Identity (Identity, runIdentity)
import PlutusLedgerApi.V1.Interval (Extended (..), Interval (..), LowerBound (..), UpperBound (..))
import PlutusLedgerApi.V1.Scripts (Datum)
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
  fromList,
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

  addTxInfoSignatory :: PubKeyHash -> t m ()

  setTxInfoInputs :: [TxInInfo] -> t m ()
  setTxInfoOutputs :: [TxOut] -> t m ()
  setTxInfoFee :: Value -> t m ()
  setTxInfoMint :: Value -> t m ()
  setTxInfoDCert :: [DCert] -> t m ()

  setTxInfoValidRange :: POSIXTimeRange -> t m ()
  setTxInfoSignatories :: [PubKeyHash] -> t m ()

  setTxInfoId :: BuiltinByteString -> t m ()

instance Monad m => TxInfoBuilder (StateT TxInfo) m where
  runTxInfoBuilder = flip execStateT
  buildTxInfo = runTxInfoBuilder emptyTxInfo
  addInput r a v d =
    let i = TxInInfo r (TxOut a v (maybe NoOutputDatum OutputDatum d) Nothing)
     in -- The Nothing indicates no refference script
        modify
          ( \txi ->
              txi
                { txInfoInputs = txInfoInputs txi <> [i]
                }
          )
  addOutput a v d =
    let i = TxOut a v (maybe NoOutputDatum OutputDatum d) Nothing
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

  addTxInfoSignatory s = modify (\txi -> txi {txInfoSignatories = txInfoSignatories txi <> [s]})

  setTxInfoInputs i = modify (\txi -> txi {txInfoInputs = i})
  setTxInfoOutputs o = modify (\txi -> txi {txInfoOutputs = o})
  setTxInfoFee f = modify (\txi -> txi {txInfoFee = f})
  setTxInfoMint m = modify (\txi -> txi {txInfoMint = m})
  setTxInfoDCert d = modify (\txi -> txi {txInfoDCert = d})

  setTxInfoValidRange r = modify (\txi -> txi {txInfoValidRange = r})
  setTxInfoSignatories s = modify (\txi -> txi {txInfoSignatories = s})

  setTxInfoId b = modify (\txi -> txi {txInfoId = TxId b})
