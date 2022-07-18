module Page.Home where

import Contract.Prelude

import Api (helloScript, redeemFromScript, sendDatumToScript, setDatumAtScript)
import Contract.Monad (Contract, DefaultContractConfig, liftContractAffM, liftContractM, runContract, runContract_)
import Contract.PlutusData (Datum(..), PlutusData(..), getDatumByHash)
import Contract.Transaction (TransactionInput, TransactionOutput(..))
import Contract.Utxos (getUtxo)
import Control.Monad.Reader (class MonadAsk, asks)
import Data.BigInt as Big
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Plutus.Types.Value (Value)
import Scripts (validatorHash)

initDatum :: Int
initDatum = 1

incrAmount :: Int
incrAmount = 2

type Payload =
  { datum :: Int
  , fundsLocked :: Value
  , lastOutput :: TransactionInput
  }

data Action
  = Lock
  | Incr Payload
  | Redeem Payload

data State
  = Unlocked
  | Locking
  | Locked Payload
  | Incrementing Int Int
  | Redeeming Value

getDatumFromState :: TransactionInput -> Contract () Int
getDatumFromState txId = do
  TransactionOutput utxo <- getUtxo txId >>= liftContractM "couldn't find utxo"
  oldDatum <-
    utxo.dataHash
      # liftContractM "utxo had not datum hash"
      >>= getDatumByHash
      >>= liftContractM "Couldn't find datum by hash"
  asBigInt <- liftContractM "datum wasn't an integer" $ case oldDatum of
    Datum (Integer n) -> Just n
    _ -> Nothing
  liftContractM "Datum was actually big. We should support this but currently don't" $ Big.toInt asBigInt

component
  :: forall q o r m
   . MonadAff m
  => MonadAsk { contractConfig :: DefaultContractConfig | r } m
  => H.Component q Unit o m
component =
  H.mkComponent
    { initialState: const Unlocked
    , render
    , eval:
        H.mkEval
          $ H.defaultEval
              { handleAction = handleAction
              }
    }
  where

  handleAction :: forall slots. Action -> H.HalogenM State Action slots o m Unit
  handleAction = case _ of
    Lock -> do
      H.modify_ \_ -> Locking
      cfg <- asks _.contractConfig
      (lastOutput /\ fundsLocked) <- H.liftAff $ runContract cfg $ do
        validator <- helloScript initDatum
        vhash <- liftContractAffM "Couldn't hash validator" $ validatorHash validator
        txId <- sendDatumToScript incrAmount vhash
        TransactionOutput utxo <- getUtxo txId >>= liftContractM "couldn't find utxo"
        pure (txId /\ utxo.amount)
      H.modify_ \_ -> Locked { datum: initDatum + incrAmount, fundsLocked, lastOutput }
    Incr { datum, fundsLocked, lastOutput } -> do
      H.modify_ \_ -> Incrementing datum (datum + 2)
      cfg <- asks _.contractConfig
      lastOutput' <- H.liftAff $ runContract cfg $ do
        validator <- helloScript datum
        vhash <- liftContractAffM "Couldn't hash validator" $ validatorHash validator
        oldDatum <- getDatumFromState lastOutput
        setDatumAtScript (oldDatum + incrAmount) vhash validator lastOutput
      H.modify_ \_ -> Locked $ { datum: datum + incrAmount, fundsLocked, lastOutput: lastOutput' }
    Redeem { datum, fundsLocked, lastOutput } -> do
      H.modify_ \_ -> Redeeming fundsLocked
      cfg <- asks _.contractConfig
      H.liftAff $ runContract_ cfg $ do
        validator <- helloScript datum
        vhash <- liftContractAffM "Couldn't hash validator" $ validatorHash validator
        redeemFromScript vhash validator lastOutput
      H.modify_ \_ -> Unlocked

  render :: forall slots. State -> H.ComponentHTML Action slots m
  render = case _ of
    Unlocked ->
      HH.main_
        [ HH.button
            [ HP.id "lock"
            , HE.onClick \_ -> Lock
            ]
            [ HH.text "Initialize"
            ]
        ]
    Locking ->
      HH.main_
        [ HH.text "Initializing"
        ]
    Locked payload@{ datum: datum, fundsLocked: fundsLocked } ->
      HH.main_
        [ HH.table_
            [ HH.tr_
                [ HH.td_ [ HH.text "Current Value" ]
                , HH.td_ [ HH.text "Funds Locked" ]
                ]
            , HH.tr_
                [ HH.td_ [ HH.text $ show datum ]
                , HH.td_ [ HH.text $ show fundsLocked <> " ADA" ]
                ]
            , HH.tr_
                [ HH.td_ [ HH.button [ HP.id "incr", HE.onClick \_ -> Incr payload ] [ HH.text "Increment" ] ]
                , HH.td_ [ HH.button [ HP.id "redeem", HE.onClick \_ -> Redeem payload ] [ HH.text "Redeem" ] ]
                ]
            ]
        ]
    Incrementing from to ->
      HH.main_
        [ HH.text $ "Incrementing " <> show from <> " to " <> show to <> "..."
        ]
    Redeeming funds ->
      HH.main_
        [ HH.text $ "Redeeming " <> show funds <> " ADA..." ]