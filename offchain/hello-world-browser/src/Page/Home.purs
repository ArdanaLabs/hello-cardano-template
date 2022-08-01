module HelloWorld.Page.Home where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Exception (Error)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import HelloWorld.Capability.HelloWorldApi (class HelloWorldApi, FundsLocked, HelloWorldIncrement(..), increment, lock, redeem)
import HelloWorld.Error (HelloWorldBrowserError(..), timeoutErrorMessage)

data Action
  = Lock
  | Increment
  | Redeem

data State
  = Unlocked
  | Locking
  | LockFailed Error
  | Locked Int FundsLocked
  | Incrementing Int Int
  | IncrementFailed Int FundsLocked Error
  | Redeeming FundsLocked
  | RedeemFailed Int FundsLocked Error
  | TransactionTimeout

helloWorldIncrement :: HelloWorldIncrement
helloWorldIncrement = HelloWorldIncrement 2

_doIncrement
  :: forall slots o m
   . MonadAff m
  => HelloWorldApi m
  => Int
  -> FundsLocked
  -> H.HalogenM State Action slots o m Unit
_doIncrement datum fundsLocked = do
  H.modify_ $ const (Incrementing datum (datum + 2))
  result <- increment helloWorldIncrement
  case result of
    Left err -> case err of
      TimeoutError -> H.modify_ $ const TransactionTimeout
      OtherError err' -> H.modify_ $ const (IncrementFailed datum fundsLocked err')
    Right _ -> H.modify_ $ const (Locked (datum + 2) fundsLocked)

_doRedeem
  :: forall slots o m
   . MonadAff m
  => HelloWorldApi m
  => Int
  -> FundsLocked
  -> H.HalogenM State Action slots o m Unit
_doRedeem datum fundsLocked = do
  H.modify_ $ const (Redeeming fundsLocked)
  result <- redeem helloWorldIncrement
  case result of
    Left err -> case err of
      TimeoutError -> H.modify_ $ const TransactionTimeout
      OtherError err' -> H.modify_ $ const (RedeemFailed datum fundsLocked err')
    Right _ -> H.modify_ $ const Unlocked

component
  :: forall q o m
   . MonadAff m
  => HelloWorldApi m
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
      H.modify_ $ const Locking
      result <- lock helloWorldIncrement 1
      case result of
        Left err -> case err of
          TimeoutError -> H.modify_ $ const TransactionTimeout
          OtherError err' -> H.modify_ $ const (LockFailed err')
        Right fundsLocked -> H.modify_ $ const (Locked 3 fundsLocked)
    Increment ->
      H.get >>= case _ of
        Locked datum fundsLocked -> _doIncrement datum fundsLocked
        IncrementFailed datum fundsLocked _ -> _doIncrement datum fundsLocked
        RedeemFailed datum fundsLocked _ -> _doIncrement datum fundsLocked
        _ -> pure unit
    Redeem ->
      H.get >>= case _ of
        Locked datum fundsLocked -> _doRedeem datum fundsLocked
        IncrementFailed datum fundsLocked _ -> _doRedeem datum fundsLocked
        RedeemFailed datum fundsLocked _ -> _doRedeem datum fundsLocked
        _ -> pure unit

  render :: forall slots. State -> H.ComponentHTML Action slots m
  render = case _ of
    TransactionTimeout ->
      HH.main_
        [ HH.text timeoutErrorMessage ]
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
      HH.main_ [ HH.text "Initializing ..." ]
    LockFailed _ ->
      HH.main_
        [ HH.button
            [ HP.id "lock"
            , HE.onClick \_ -> Lock
            ]
            [ HH.text "Initialize"
            ]
        ]
    Locked datum fundsLocked ->
      lockedView datum fundsLocked Nothing
    Incrementing from to ->
      HH.main_
        [ HH.text $ "Incrementing from " <> show from <> " to " <> show to <> " ..." ]
    IncrementFailed datum fundsLocked err ->
      lockedView datum fundsLocked (Just err)
    Redeeming funds ->
      HH.main_
        [ HH.text $ "Redeeming " <> show funds <> " ADA ..." ]
    RedeemFailed datum fundsLocked err ->
      lockedView datum fundsLocked (Just err)
    where
    lockedView datum fundsLocked _ =
      HH.main_
        [ HH.table_
            [ HH.thead_
                [ HH.tr_
                    [ HH.td [ HP.id "current-value-header" ] [ HH.text "Current Value" ]
                    , HH.td [ HP.id "funds-locked-header" ] [ HH.text "Funds Locked" ]
                    ]
                ]
            , HH.tbody_
                [ HH.tr_
                    [ HH.td [ HP.id "current-value-body" ] [ HH.text $ show datum ]
                    , HH.td [ HP.id "funds-locked-body" ] [ HH.text $ show fundsLocked <> " ADA" ]
                    ]
                ]
            , HH.tfoot_
                [ HH.tr_
                    [ HH.td_ [ HH.button [ HP.id "increment", HE.onClick \_ -> Increment ] [ HH.text "+" ] ]
                    , HH.td_ [ HH.button [ HP.id "redeem", HE.onClick \_ -> Redeem ] [ HH.text "Redeem" ] ]
                    ]
                ]
            ]
        ]
