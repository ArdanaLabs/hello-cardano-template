module HelloWorld.Page.Home where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import HelloWorld.Capability.CardanoApi (class CardanoApi, enable)
import HelloWorld.Capability.HelloWorldApi (class HelloWorldApi, FundsLocked, HelloWorldIncrement(..), getDatum, increment, lock, redeem, resume, unlock)
import HelloWorld.Error (HelloWorldBrowserError(..))
import Web.HTML (window)
import Web.HTML.Location (reload)
import Web.HTML.Window (alert, location)

data Action
  = Lock
  | Increment
  | Redeem
  | Resume
  | Enable

data State
  = Enabling
  | EnableFailed HelloWorldBrowserError
  | Unlocked
  | Locking
  | LockFailed HelloWorldBrowserError
  | Locked Int FundsLocked
  | Incrementing Int Int
  | IncrementFailed Int FundsLocked HelloWorldBrowserError
  | Redeeming FundsLocked
  | RedeemFailed Int FundsLocked HelloWorldBrowserError
  | Unlocking
  | Resuming
  | ResumeFailed HelloWorldBrowserError

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
  let HelloWorldIncrement inc = helloWorldIncrement
  H.modify_ $ const (Incrementing datum (datum + inc))
  result <- increment helloWorldIncrement
  case result of
    Left NetworkChanged -> H.liftEffect do
      window >>= \w -> do
        alert (show NetworkChanged) w
        location w >>= reload
    Left err -> H.modify_ $ const (IncrementFailed datum fundsLocked err)
    Right _ -> getDatum >>= case _ of
      Left NetworkChanged -> H.liftEffect do
        window >>= \w -> do
          alert (show NetworkChanged) w
          location w >>= reload
      Left err -> H.modify_ $ const (IncrementFailed datum fundsLocked err)
      Right new -> do
        H.modify_ $ const (Locked new fundsLocked)

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
    Left NetworkChanged -> H.liftEffect do
      window >>= \w -> do
        alert (show NetworkChanged) w
        location w >>= reload
    Left err -> handleError err
    Right balanceBeforeRedeem -> do
      H.modify_ $ const Unlocking
      result' <- unlock balanceBeforeRedeem
      case result' of
        Left NetworkChanged -> H.liftEffect do
          window >>= \w -> do
            alert (show NetworkChanged) w
            location w >>= reload
        Left err -> handleError err
        Right _ -> H.modify_ $ const Unlocked
  where
  handleError err = H.modify_ $ const (RedeemFailed datum fundsLocked err)

component
  :: forall q o m
   . MonadAff m
  => CardanoApi m
  => HelloWorldApi m
  => H.Component q Unit o m
component =
  H.mkComponent
    { initialState: const Enabling
    , render
    , eval:
        H.mkEval
          $ H.defaultEval
              { handleAction = handleAction
              , initialize = Just Enable
              }
    }
  where

  handleAction :: forall slots. Action -> H.HalogenM State Action slots o m Unit
  handleAction = case _ of
    Enable -> do
      result <- enable
      case result of
        Left err -> H.modify_ $ const (EnableFailed err)
        Right _ -> H.modify_ $ const Unlocked
    Resume -> do
      H.modify_ $ const Resuming
      result <- resume helloWorldIncrement
      case result of
        Left NetworkChanged -> H.liftEffect do
          window >>= \w -> do
            alert (show NetworkChanged) w
            location w >>= reload
        Left err -> H.modify_ $ const (LockFailed err)
        Right funds -> do
          getDatum >>= case _ of
            Left err -> H.modify_ $ const (ResumeFailed err)
            Right new -> H.modify_ $ const (Locked new funds)
      -- TODO actually query funds locked
      pure unit
    Lock -> do
      H.modify_ $ const Locking
      let init = 1
      result <- lock helloWorldIncrement init
      case result of
        Left NetworkChanged -> H.liftEffect do
          window >>= \w -> do
            alert (show NetworkChanged) w
            location w >>= reload
        Left err -> H.modify_ $ const (LockFailed err)
        Right fundsLocked ->
          getDatum >>= case _ of
            Left err -> H.modify_ $ const (LockFailed err)
            Right new -> H.modify_ $ const (Locked new fundsLocked)
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
    Enabling ->
      HH.main_
        [ HH.text "Enabling wallet ..."
        ]
    EnableFailed err ->
      HH.main_
        [ HH.p
            [ HP.class_ $ ClassName "error" ]
            [ HH.text (show err) ]
        ]
    ResumeFailed err ->
      HH.main_
        [ HH.p
            [ HP.class_ $ ClassName "error" ]
            [ HH.text (show err) ]
        , HH.button
            [ HP.id "lock"
            , HE.onClick \_ -> Lock
            ]
            [ HH.text "Initialize"
            ]
        , HH.button
            [ HP.id "resume"
            , HE.onClick \_ -> Resume
            ]
            [ HH.text "Resume"
            ]
        ]
    Resuming ->
      HH.main_ [ HH.text "Resuming ..." ]
    Unlocked ->
      HH.main_
        [ HH.button
            [ HP.id "lock"
            , HE.onClick \_ -> Lock
            ]
            [ HH.text "Initialize"
            ]
        , HH.button
            [ HP.id "resume"
            , HE.onClick \_ -> Resume
            ]
            [ HH.text "Resume"
            ]
        ]
    Locking ->
      HH.main_ [ HH.text "Initializing ..." ]
    LockFailed err -> do
      HH.main_
        [ HH.p
            [ HP.class_ $ ClassName "error" ]
            [ HH.text (show err) ]
        , HH.button
            [ HP.id "lock"
            , HE.onClick \_ -> Lock
            ]
            [ HH.text "Initialize"
            ]
        , HH.button
            [ HP.id "resume"
            , HE.onClick \_ -> Resume
            ]
            [ HH.text "Resume"
            ]
        ]
    Locked datum fundsLocked ->
      HH.main_
        [ lockedView datum fundsLocked
        ]
    Incrementing from to ->
      HH.main_
        [ HH.text $ "Incrementing from " <> show from <> " to " <> show to <> " ..." ]
    IncrementFailed datum fundsLocked err ->
      HH.main_
        [ HH.text $ show err
        , lockedView datum fundsLocked
        ]
    Redeeming funds ->
      HH.main_
        [ HH.text $ "Redeeming " <> show funds <> " ADA ..." ]
    RedeemFailed datum fundsLocked err ->
      HH.main_
        [ HH.text $ show err
        , lockedView datum fundsLocked
        ]
    Unlocking ->
      HH.main_
        [ HH.div
            [ HP.id "unlocking" ]
            [ HH.text $ "Unlocking funds ..." ]
        ]
    where
    lockedView datum fundsLocked =
      HH.table_
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

