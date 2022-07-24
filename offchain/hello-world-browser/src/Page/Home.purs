module HelloWorld.Page.Home where

import Prelude

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
  | Locked Int FundsLocked
  | Incrementing Int Int
  | Redeeming FundsLocked

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
      fundsLocked <- lock (ScriptAddress 2) 1
      H.modify_ $ const $ Locked 3 fundsLocked
    Increment ->
      H.get >>= case _ of
        Locked datum fundsLocked -> do
          H.modify_ $ const $ Incrementing datum (datum + 2)
          increment (ScriptAddress 2) 2
          H.modify_ $ const $ Locked (datum + 2) fundsLocked
        _ -> pure unit
    Redeem ->
      H.get >>= case _ of
        Locked _ fundsLocked -> do
          H.modify_ $ const $ Redeeming fundsLocked
          redeem (ScriptAddress 2)
          H.modify_ $ const Unlocked
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
      HH.main_
        [ HH.text "Initializing"
        ]
    Locked datum fundsLocked ->
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
                [ HH.td_ [ HH.button [ HP.id "increment", HE.onClick \_ -> Increment ] [ HH.text "Increment" ] ]
                , HH.td_ [ HH.button [ HP.id "redeem", HE.onClick \_ -> Redeem ] [ HH.text "Redeem" ] ]
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