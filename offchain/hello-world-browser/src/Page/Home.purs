module HelloWorld.Page.Home where

import Contract.Prelude

import Contract.Transaction (TransactionInput)
import Effect.Aff.Class (class MonadAff)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import HelloWorld.Capability.CardanoApi (class CardanoApi, enable)
import HelloWorld.Capability.HelloWorldApi (class HelloWorldApi, FundsLocked(..), HelloWorldIncrement(..), getDatum, increment, lock, redeem, resume, unlock)
import HelloWorld.Error (HelloWorldBrowserError(..))
import HelloWorld.Types (HelloWorldWallet)
import Web.HTML (window)
import Web.HTML.Location (reload)
import Web.HTML.Window (alert, location)

data Action
  = Lock
  | Increment
  | Redeem
  | Resume
  | Enable HelloWorldWallet

data HelloWorldBrowser
  = Wallets (Array HelloWorldWallet)
  | Enabling
  | EnableFailed HelloWorldBrowserError
  | Unlocked
  | Locking
  | LockFailed HelloWorldBrowserError
  | Locked
  | Incrementing
  | IncrementFailed HelloWorldBrowserError
  | Redeeming
  | RedeemFailed HelloWorldBrowserError
  | Unlocking
  | Resuming
  | ResumeFailed HelloWorldBrowserError

type State =
  { helloWorldBrowser :: HelloWorldBrowser
  , lastOutput :: Maybe TransactionInput
  , datum :: Int
  , value :: FundsLocked
  }

helloWorldIncrement :: HelloWorldIncrement
helloWorldIncrement = HelloWorldIncrement 2

alertAndReload :: String -> Effect Unit
alertAndReload err =
  window >>= \w -> do
    alert err w
    location w >>= reload

_doIncrement
  :: forall slots o m
   . MonadAff m
  => HelloWorldApi m
  => TransactionInput
  -> H.HalogenM State Action slots o m Unit
_doIncrement lastOutput = do
  H.modify_ _ { helloWorldBrowser = Incrementing }
  increment helloWorldIncrement lastOutput >>= case _ of
    Left NetworkChanged -> H.liftEffect $ alertAndReload (show NetworkChanged)
    Left err -> H.modify_ _ { helloWorldBrowser = IncrementFailed err }
    Right lastOutput' -> getDatum lastOutput' >>= case _ of
      Left NetworkChanged -> H.liftEffect $ alertAndReload (show NetworkChanged)
      Left err -> H.modify_ _ { helloWorldBrowser = IncrementFailed err }
      Right new -> H.modify_ _ { datum = new, lastOutput = Just lastOutput', helloWorldBrowser = Locked }

_doRedeem
  :: forall slots o m
   . MonadAff m
  => HelloWorldApi m
  => TransactionInput
  -> H.HalogenM State Action slots o m Unit
_doRedeem lastOutput = do
  H.modify_ _ { helloWorldBrowser = Redeeming }
  redeem helloWorldIncrement lastOutput >>= case _ of
    Left NetworkChanged -> H.liftEffect $ alertAndReload (show NetworkChanged)
    Left err -> H.modify_ _ { helloWorldBrowser = RedeemFailed err }
    Right balanceBeforeRedeem -> do
      H.modify_ _ { helloWorldBrowser = Unlocking }
      unlock balanceBeforeRedeem >>= case _ of
        Left NetworkChanged -> H.liftEffect $ alertAndReload (show NetworkChanged)
        Left err -> H.modify_ _ { helloWorldBrowser = RedeemFailed err }
        Right _ -> H.modify_ _ { helloWorldBrowser = Unlocked, lastOutput = Nothing, datum = 0, value = FundsLocked 0.0 }

component
  :: forall q o m
   . MonadAff m
  => CardanoApi m
  => HelloWorldApi m
  => H.Component q HelloWorldBrowser o m
component =
  H.mkComponent
    { initialState: \helloWorldBrowser ->
        { helloWorldBrowser, lastOutput: Nothing, datum: 0, value: FundsLocked 0.0 }
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
    Enable wallet -> do
      H.modify_ _ { helloWorldBrowser = Enabling }
      enable wallet >>= case _ of
        Left err -> H.modify_ _ { helloWorldBrowser = EnableFailed err }
        Right _ -> H.modify_ _ { helloWorldBrowser = Unlocked }
    Resume -> do
      H.modify_ _ { helloWorldBrowser = Resuming }
      resume helloWorldIncrement >>= case _ of
        Left NetworkChanged -> H.liftEffect $ alertAndReload (show NetworkChanged)
        Left err -> H.modify_ _ { helloWorldBrowser = LockFailed err }
        Right (lastOutput /\ fundsLocked) ->
          getDatum lastOutput >>= case _ of
            Left err -> H.modify_ _ { helloWorldBrowser = ResumeFailed err }
            Right new -> H.modify_ _ { lastOutput = Just lastOutput, helloWorldBrowser = Locked, datum = new, value = fundsLocked }
      -- TODO actually query funds locked
      pure unit
    Lock -> do
      H.modify_ _ { helloWorldBrowser = Locking }
      lock helloWorldIncrement 1 >>= case _ of
        Left NetworkChanged -> H.liftEffect $ alertAndReload (show NetworkChanged)
        Left err -> H.modify_ _ { helloWorldBrowser = LockFailed err }
        Right (lastOutput /\ fundsLocked) ->
          getDatum lastOutput >>= case _ of
            Left err -> H.modify_ _ { helloWorldBrowser = LockFailed err }
            Right new -> H.modify_ _ { lastOutput = Just lastOutput, helloWorldBrowser = Locked, datum = new, value = fundsLocked }
    Increment -> do
      { lastOutput, helloWorldBrowser } <- H.get
      case lastOutput of
        Nothing -> pure unit
        Just lastOutput' -> case helloWorldBrowser of
          Locked -> _doIncrement lastOutput'
          IncrementFailed _ -> _doIncrement lastOutput'
          RedeemFailed _ -> _doIncrement lastOutput'
          _ -> pure unit
    Redeem -> do
      { lastOutput, helloWorldBrowser } <- H.get
      case lastOutput of
        Nothing -> pure unit
        Just lastOutput' -> case helloWorldBrowser of
          Locked -> _doRedeem lastOutput'
          IncrementFailed _ -> _doRedeem lastOutput'
          RedeemFailed _ -> _doRedeem lastOutput'
          _ -> pure unit

  render :: forall slots. State -> H.ComponentHTML Action slots m
  render { helloWorldBrowser, datum, value } = case helloWorldBrowser of
    Wallets wallets ->
      HH.main_
        [ if (null wallets) then
            HH.p
              [ HP.class_ $ ClassName "error"
              ]
              [ HH.text "No wallets found, please install at least one wallet and retry." ]
          else
            HH.dt_
              ( wallets <#> \wallet ->
                  HH.dd_
                    [ HH.button
                        [ HE.onClick $ \_ -> Enable wallet ]
                        [ HH.text $ "Use " <> (show wallet) ]
                    ]
              )
        ]
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
    Locked ->
      HH.main_
        [ lockedView
        ]
    Incrementing -> do
      let HelloWorldIncrement inc = helloWorldIncrement
      HH.main_
        [ HH.text $ "Incrementing from " <> show datum <> " to " <> show (datum + inc) <> " ..." ]
    IncrementFailed err ->
      HH.main_
        [ HH.p
            [ HP.class_ $ ClassName "error" ]
            [ HH.text (show err) ]
        , lockedView
        ]
    Redeeming ->
      HH.main_
        [ HH.text $ "Redeeming " <> show value <> " ADA ..." ]
    RedeemFailed err ->
      HH.main_
        [ HH.p
            [ HP.class_ $ ClassName "error" ]
            [ HH.text (show err) ]
        , lockedView
        ]
    Unlocking ->
      HH.main_
        [ HH.div
            [ HP.id "unlocking" ]
            [ HH.text $ "Unlocking funds ..." ]
        ]
    where
    lockedView =
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
                , HH.td [ HP.id "funds-locked-body" ] [ HH.text $ show value <> " ADA" ]
                ]
            ]
        , HH.tfoot_
            [ HH.tr_
                [ HH.td_ [ HH.button [ HP.id "increment", HE.onClick \_ -> Increment ] [ HH.text "+" ] ]
                , HH.td_ [ HH.button [ HP.id "redeem", HE.onClick \_ -> Redeem ] [ HH.text "Redeem" ] ]
                ]
            ]
        ]

