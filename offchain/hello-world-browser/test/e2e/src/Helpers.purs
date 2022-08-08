module HelloWorld.Test.E2E.Helpers where

import Prelude

import Contract.Test.E2E (Mode(..), RunningExample, TestOptions(..), WalletExt(..), delaySec, namiSign, withExample)
import Data.Either (Either(..))
import Data.Maybe (fromMaybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Effect.Aff (Aff, bracket, try)
import Effect.Class (liftEffect)
import Foreign (Foreign, unsafeFromForeign)
import Foreign as Foreign
import HelloWorld.Test.E2E.Constants as Constants
import Mote (test)
import Node.Express.App (listenHttp, use)
import Node.Express.Middleware.Static (static)
import Node.HTTP (close, Server)
import Node.Path (FilePath)
import TestM (TestPlanM)
import Toppokki as T

-- | A String representing a jQuery selector, e.g. "#my-id" or ".my-class"
newtype Selector = Selector String

derive instance Newtype Selector _
-- | A String representing a jQuery action, e.g. "click" or "enable".
newtype Action = Action String

derive instance Newtype Action _

helloWorldBrowserURL :: T.URL
helloWorldBrowserURL = T.URL $ Constants.localhost <> ":" <> show Constants.port

-- | Run an E2E test. Parameters are:
-- |   String: Just a name for the logs
-- |   Toppokki.URL: URL where the example is running
-- |   TestOptions: Options to start the browser with
-- |   WalletExt: An extension which should be used
-- |   RunningExample -> Aff a: A function which runs the test
runE2ETest
  :: forall (a :: Type)
   . String
  -> TestOptions
  -> WalletExt
  -> (RunningExample -> Aff a)
  -> TestPlanM Unit
runE2ETest example opts ext f = test example $ withBrowser opts ext $
  \browser -> withExample helloWorldBrowserURL browser $ void <<< f

launchWithExtension :: WalletExt -> TestOptions -> Aff T.Browser
launchWithExtension walletExt testOptions@(TestOptions { chromeExe, chromeUserDataDir, namiDir, geroDir, noHeadless }) = do
  result <- try $ T.launch
    { args:
        [ "--disable-extensions-except=" <> extDir
        , "--load-extension=" <> extDir
        ] <> if mode == Headless then [ "--headless=chrome" ] else []
    , headless: mode == Headless
    , userDataDir: chromeUserDataDir
    , executablePath: fromMaybe "" chromeExe
    }
  case result of
    Left _ -> do
      delaySec Constants.tenSeconds
      launchWithExtension walletExt testOptions
    Right browser -> pure browser
  where
  mode :: Mode
  mode
    | noHeadless = Visible
    | otherwise = Headless

  extDir :: FilePath
  extDir = case walletExt of
    GeroExt -> geroDir
    NamiExt -> namiDir

withBrowser
  :: forall (a :: Type)
   . TestOptions
  -> WalletExt
  -> (T.Browser -> Aff a)
  -> Aff a
withBrowser opts ext = bracket (launchWithExtension ext opts) T.close

-- | Build a primitive jQuery expression like '$("button").click()' and
-- | out of a selector and action and evaluate it in Toppokki
doJQ :: Selector -> Action -> T.Page -> Aff Foreign
doJQ selector action page = do
  T.unsafeEvaluateStringFunction jq page
  where
  jq :: String
  jq = "$('" <> unwrap selector <> "')." <> unwrap action

-- | select a button with a specific text inside
buttonWithText :: String -> Selector
buttonWithText text = wrap $ "button:contains(" <> text <> ")"

click :: Action
click = wrap "click()"

clickButton :: String -> T.Page -> Aff Unit
clickButton buttonText = void <$> doJQ (buttonWithText buttonText) click

injectJQuery :: String -> T.Page -> Aff Unit
injectJQuery jQuery page = do
  (alreadyInjected :: Boolean) <-
    unsafeFromForeign <$>
      T.unsafeEvaluateStringFunction "typeof(jQuery) !== 'undefined'"
        page
  unless alreadyInjected $ void $ T.unsafeEvaluateStringFunction jQuery
    page

namiSign' :: RunningExample -> Aff Unit
namiSign' = namiSign Constants.namiWalletPassword

startStaticServer :: String -> Aff Server
startStaticServer directory =
  liftEffect $ listenHttp (use $ static directory) Constants.port $ \_ -> pure unit

closeStaticServer :: Server -> Aff Unit
closeStaticServer server = liftEffect $ close server (pure unit)

getCurrentValueHeader :: String
getCurrentValueHeader = "[].map.call(document.querySelectorAll('#current-value-header'), el => el.textContent)"

getCurrentValueBody :: String
getCurrentValueBody = "[].map.call(document.querySelectorAll('#current-value-body'), el => el.textContent)"

getFundsLockedHeader :: String
getFundsLockedHeader = "[].map.call(document.querySelectorAll('#funds-locked-header'), el => el.textContent)"

getFundsLockedBody :: String
getFundsLockedBody = "[].map.call(document.querySelectorAll('#funds-locked-body'), el => el.textContent)"

readString :: Foreign -> String
readString = Foreign.unsafeFromForeign