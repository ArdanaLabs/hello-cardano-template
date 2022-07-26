module HelloWorld.Page.Home where

import Prelude

import Data.Either (Either(..))
import Effect.Aff (Error, message)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import HelloWorld.Capability.HelloWorldApi (class HelloWorldApi, FundsLocked, ScriptAddress(..), increment, lock, redeem)

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

script :: ScriptAddress
script = ScriptAddress 2

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
      result <- lock script 1
      case result of
        Left err -> H.modify_ $ const (LockFailed err)
        Right fundsLocked -> H.modify_ $ const (Locked 3 fundsLocked)
    Increment ->
      H.get >>= case _ of
        Locked datum fundsLocked -> do
          H.modify_ $ const (Incrementing datum (datum + 2))
          result <- increment script 2
          case result of
            Left err -> H.modify_ $ const (IncrementFailed datum fundsLocked err)
            Right _ -> H.modify_ $ const (Locked (datum + 2) fundsLocked)
        _ -> pure unit
    Redeem ->
      H.get >>= case _ of
        Locked datum fundsLocked -> do
          H.modify_ $ const (Redeeming fundsLocked)
          result <- redeem script
          case result of
            Left err -> H.modify_ $ const (RedeemFailed datum fundsLocked err)
            Right _ -> H.modify_ $ const Unlocked
        _ -> pure unit

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
      HH.main_ [ HH.text "Initializing" ]
    LockFailed err ->
      HH.main_
        [ HH.text $ message err
        , HH.button
            [ HP.id "lock"
            , HE.onClick \_ -> Lock
            ]
            [ HH.text "Initialize"
            ]
        ]
    Locked datum fundsLocked ->
      HH.main_
        [ HH.table_
            [ HH.tr_
                [ HH.td [ HP.id "current-value-header" ] [ HH.text "Current Value" ]
                , HH.td [ HP.id "funds-locked-header" ] [ HH.text "Funds Locked" ]
                ]
            , HH.tr_
                [ HH.td [ HP.id "current-value-body" ] [ HH.text $ show datum ]
                , HH.td [ HP.id "funds-locked-body" ] [ HH.text $ show fundsLocked <> " ADA" ]
                ]
            , HH.tr_
                [ HH.td_ [ HH.button [ HP.id "increment", HE.onClick \_ -> Increment ] [ HH.text "Increment" ] ]
                , HH.td_ [ HH.button [ HP.id "redeem", HE.onClick \_ -> Redeem ] [ HH.text "Redeem" ] ]
                ]
            ]
        ]
    Incrementing from to ->
      HH.main_
        [ HH.text $ "Incrementing from " <> show from <> " to " <> show to <> " ..." ]
    IncrementFailed datum fundsLocked err ->
      HH.main_
        [ HH.text $ message err
        , HH.table_
            [ HH.tr_
                [ HH.td [ HP.id "current-value-header" ] [ HH.text "Current Value" ]
                , HH.td [ HP.id "funds-locked-header" ] [ HH.text "Funds Locked" ]
                ]
            , HH.tr_
                [ HH.td [ HP.id "current-value-body" ] [ HH.text $ show datum ]
                , HH.td [ HP.id "funds-locked-body" ] [ HH.text $ show fundsLocked <> " ADA" ]
                ]
            , HH.tr_
                [ HH.td_ [ HH.button [ HP.id "increment", HE.onClick \_ -> Increment ] [ HH.text "Increment" ] ]
                , HH.td_ [ HH.button [ HP.id "redeem", HE.onClick \_ -> Redeem ] [ HH.text "Redeem" ] ]
                ]
            ]
        ]
    Redeeming funds ->
      HH.main_
        [ HH.text $ "Redeeming " <> show funds <> " ADA ..." ]
    RedeemFailed datum fundsLocked err ->
      HH.main_
        [ HH.text $ message err
        , HH.table_
            [ HH.tr_
                [ HH.td [ HP.id "current-value-header" ] [ HH.text "Current Value" ]
                , HH.td [ HP.id "funds-locked-header" ] [ HH.text "Funds Locked" ]
                ]
            , HH.tr_
                [ HH.td [ HP.id "current-value-body" ] [ HH.text $ show datum ]
                , HH.td [ HP.id "funds-locked-body" ] [ HH.text $ show fundsLocked <> " ADA" ]
                ]
            , HH.tr_
                [ HH.td_ [ HH.button [ HP.id "increment", HE.onClick \_ -> Increment ] [ HH.text "Increment" ] ]
                , HH.td_ [ HH.button [ HP.id "redeem", HE.onClick \_ -> Redeem ] [ HH.text "Redeem" ] ]
                ]
            ]
        ]