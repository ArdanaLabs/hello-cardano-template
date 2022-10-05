module HelloWorld.Test.NamiWallet
  ( ChromeUserDataDir(..)
  , NamiDir(..)
  , NamiWallet(..)
  , buttonWithText
  , byId
  , click
  , clickButton
  , doJQ
  , findWalletPage
  , hasSelector
  , inWalletPage
  , injectJQuery
  , inputType
  , launchWithExtension
  , mkChromeUserDataDir
  , mkNamiDir
  , mkNamiWallet
  , mkTempDir
  , mkTestOptions
  , namiConfirmAccess'
  , namiSign
  , namiSign'
  , pageUrl
  , password
  , reactSetValue
  , runE2ETest
  , testWallet1
  , testWallet2
  , testWallet3
  , topup
  , unzipNamiExtension
  , unzipNamiSettings
  , waitForWalletPage
  , withBrowser
  ) where

import Contract.Prelude

import Contract.Test.E2E (Mode(..), RunningExample, TestOptions(..), WalletConfig(..), WalletExt(..), WalletPassword, delaySec, namiConfirmAccess, walletName, withExample)
import Contract.Test.E2E.Helpers (ExtensionId(..))
import Data.Array (filterA, head)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.String (Pattern(..), trim)
import Data.String as String
import Effect (Effect)
import Effect.Aff (bracket, try)
import Effect.Exception (throw)
import Faucet as Faucet
import Foreign (Foreign, unsafeFromForeign)
import HelloWorld.Test.Constants (PaymentAddress, namiExtensionId, namiWalletPassword)
import HelloWorld.Test.Constants as Constants
import HelloWorld.Test.Env as Env
import HelloWorld.Test.Helpers (Action, Selector, helloWorldBrowserURL)
import Mote (test)
import Node.Buffer as Buffer
import Node.ChildProcess (defaultExecSyncOptions, execSync)
import Node.Encoding (Encoding(UTF8))
import Node.Path (FilePath)
import Node.Process (lookupEnv)
import TestM (TestPlanM)
import Toppokki as T

data NamiWallet = NamiWallet
  { paymentAddress :: PaymentAddress
  , settingsFile :: String
  }

mkNamiWallet :: PaymentAddress -> String -> Effect NamiWallet
mkNamiWallet paymentAddress env = do
  lookupEnv env >>= case _ of
    Nothing -> throw $ env <> " not found"
    Just settingsFile -> pure $ NamiWallet
      { paymentAddress
      , settingsFile
      }

testWallet1 :: Effect NamiWallet
testWallet1 = mkNamiWallet Constants.paymentAddressForTestWallet1 Env.namiWallet1

testWallet2 :: Effect NamiWallet
testWallet2 = mkNamiWallet Constants.paymentAddressForTestWallet2 Env.namiWallet2

testWallet3 :: Effect NamiWallet
testWallet3 = mkNamiWallet Constants.paymentAddressForTestWallet3 Env.namiWallet3

mkTempDir :: Effect String
mkTempDir = do
  buf <- execSync "mktemp --directory" defaultExecSyncOptions
  trim <$> Buffer.toString UTF8 buf

unzipNamiSettings :: String -> String -> Effect Unit
unzipNamiSettings tmpDir settingsFile =
  void $ execSync ("tar zxf " <> settingsFile <> " --directory " <> tmpDir) defaultExecSyncOptions

unzipNamiExtension :: String -> Effect Unit
unzipNamiExtension tmpDir = do
  namiExtension <- lookupEnv Env.namiExtension
  case namiExtension of
    Nothing -> throw "NAMI_EXTENSION not set"
    Just extension ->
      void $ execSync ("unzip " <> extension <> " -d " <> tmpDir <> "/nami > /dev/zero || echo \"ignore warnings\"") defaultExecSyncOptions

topup :: NamiWallet -> Effect Unit
topup (NamiWallet { paymentAddress }) = Faucet.topup $ unwrap paymentAddress

mkTestOptions :: NamiWallet -> Effect TestOptions
mkTestOptions testWallet = do
  chromeExe <- lookupEnv Env.chromeExe
  NamiDir namiDir <- mkNamiDir
  ChromeUserDataDir chromeUserDataDir <- mkChromeUserDataDir testWallet

  let
    wallets = Map.fromFoldable
      [ mkConfig NamiExt namiDir namiWalletPassword
      ]

  pure $ TestOptions
    { chromeExe
    , wallets
    , chromeUserDataDir
    , noHeadless: false
    }
  where
  mkConfig
    :: WalletExt
    -> FilePath
    -> WalletPassword
    -> Tuple WalletExt WalletConfig
  mkConfig ext fp pw = ext /\ WalletConfig fp pw

newtype NamiDir = NamiDir String

newtype ChromeUserDataDir = ChromeUserDataDir String

mkNamiDir :: Effect NamiDir
mkNamiDir = do
  tempDir <- mkTempDir
  unzipNamiExtension tempDir
  pure (NamiDir $ tempDir <> "/nami")

mkChromeUserDataDir :: NamiWallet -> Effect ChromeUserDataDir
mkChromeUserDataDir (NamiWallet { settingsFile }) = do
  tempDir <- mkTempDir
  unzipNamiSettings tempDir settingsFile
  pure (ChromeUserDataDir $ tempDir <> "/test-data/chrome-user-data")

runE2ETest
  :: forall (a :: Type)
   . String
  -> TestOptions
  -> WalletExt
  -> (RunningExample -> Aff a)
  -> TestPlanM Unit
runE2ETest example opts ext f = test example $ withBrowser opts ext $
  \mBrowser -> case mBrowser of
    Nothing -> liftEffect $ log $ "Wallet " <> walletName ext <> " not provided"
    Just browser -> withExample helloWorldBrowserURL browser $ void <<< f

launchWithExtension :: WalletExt -> TestOptions -> Aff (Maybe T.Browser)
launchWithExtension walletExt testOptions@(TestOptions { chromeExe, wallets, chromeUserDataDir, noHeadless }) =
  case Map.lookup walletExt wallets of
    Nothing -> pure Nothing
    Just (WalletConfig path _) -> do
      result <- try $ T.launch
        { args:
            [ "--disable-extensions-except=" <> path
            , "--load-extension=" <> path
            ] <> if mode == Headless then [ "--headless=chrome" ] else []
        , headless: mode == Headless
        , userDataDir: chromeUserDataDir
        , executablePath: fromMaybe "" chromeExe
        }
      case result of
        Left _ -> do
          delaySec Constants.tenSeconds
          launchWithExtension walletExt testOptions
        Right browser -> pure $ Just browser
  where
  mode :: Mode
  mode
    | noHeadless = Visible
    | otherwise = Headless

withBrowser
  :: forall (a :: Type)
   . TestOptions
  -> WalletExt
  -> (Maybe T.Browser -> Aff a)
  -> Aff a
withBrowser opts ext = bracket (launchWithExtension ext opts)
  ( \mbrowser -> case mbrowser of
      Nothing -> pure unit
      Just b -> T.close b
  )

jQueryCount :: Selector -> T.Page -> Aff Int
jQueryCount selector page = unsafeFromForeign <$> doJQ selector (wrap "length")
  page

-- | Check if a selector is matched on a page
hasSelector :: Selector -> T.Page -> Aff Boolean
hasSelector selector page = (_ > 0) <$> jQueryCount selector page

button :: Selector
button = wrap "button"

findWalletPage :: ExtensionId -> T.Browser -> Aff (Maybe T.Page)
findWalletPage (ExtensionId extId) browser = do
  pages <- T.pages browser
  head <<< fold <$> for pages \page -> do
    url <- pageUrl page
    pure $
      if String.contains (Pattern extId) url then [ page ]
      else []

pageUrl :: T.Page -> Aff String
pageUrl page = do
  unsafeFromForeign <$> T.unsafeEvaluateStringFunction
    "document.location.href"
    page

waitForWalletPage
  :: ExtensionId -> Number -> T.Browser -> Aff T.Page
waitForWalletPage extId timeout browser =
  findWalletPage extId browser >>= case _ of
    Nothing -> do
      if timeout > 0.0 then do
        delaySec 0.1
        waitForWalletPage extId (timeout - 0.1) browser
      else liftEffect $ throw
        $ "Wallet popup did not open. Did you provide extension ID correctly? "
        <> "Provided ID: "
        <> unwrap extId

    Just page -> pure page

injectJQuery :: T.Page -> String -> Aff Unit
injectJQuery page jQuery = do
  (alreadyInjected :: Boolean) <- unsafeFromForeign <$>
    T.unsafeEvaluateStringFunction "typeof(jQuery) !== 'undefined'"
      page
  unless alreadyInjected $ void $ T.unsafeEvaluateStringFunction jQuery
    page

inWalletPage
  :: forall (a :: Type)
   . ExtensionId
  -> RunningExample
  -> (T.Page -> Aff a)
  -> Aff a
inWalletPage extId { browser, jQuery } cont = do
  page <- waitForWalletPage extId 10.0 browser
  injectJQuery page jQuery
  cont page

namiSign :: ExtensionId -> WalletPassword -> RunningExample -> Aff Unit
namiSign extId wpassword re = do
  inWalletPage extId re \nami -> do
    clickButton "Sign" nami
    reactSetValue password (unwrap wpassword) nami
    clickButton "Confirm" nami

reactSetValue :: Selector -> String -> T.Page -> Aff Unit
reactSetValue selector value page = do
  let
    js = fold
      [ "let input = $('" <> unwrap selector <> "').get(0);"
      , "var nativeInputValueSetter = "
          <>
            " Object.getOwnPropertyDescriptor(window.HTMLInputElement.prototype, 'value').set;"
      , "nativeInputValueSetter.call(input, '" <> value <> "');"
      , "var ev2 = new Event('input', { bubbles: true});"
      , "input.dispatchEvent(ev2);"
      ]
  void $ T.unsafeEvaluateStringFunction js page

namiSign' :: RunningExample -> Aff Unit
namiSign' = namiSign namiExtensionId namiWalletPassword

namiConfirmAccess' :: RunningExample -> Aff Unit
namiConfirmAccess' = namiConfirmAccess namiExtensionId

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

-- | select an input element of a specific type
inputType :: String -> Selector
inputType typ = wrap $ "input[type=\"" <> typ <> "\"]"

-- | select an element by Id
byId :: String -> Selector
byId = wrap <<< ("#" <> _)

-- | select any password field
password :: Selector
password = wrap ":password"

click :: Action
click = wrap "click()"

clickButton :: String -> T.Page -> Aff Unit
clickButton btnText page = do
  let
    btn = buttonWithText btnText

  hasBtn <- hasSelector btn page

  if hasBtn then
    void $ doJQ btn click page
  else do
    delaySec 0.1
    clickButton btnText page
