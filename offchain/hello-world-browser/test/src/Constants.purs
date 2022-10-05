module HelloWorld.Test.Constants where

import Contract.Test.E2E (WalletPassword)
import Contract.Test.E2E.Helpers (ExtensionId(..))
import Data.Newtype (class Newtype, wrap)
import Node.Express.Types (Port)

port :: Port
port = 6789

localhost :: String
localhost = "http://127.0.0.1"

namiWalletPassword :: WalletPassword
namiWalletPassword = wrap "ctlctlctl"

namiExtensionId :: ExtensionId
namiExtensionId = ExtensionId "lpfcbjknijpeeillifnkikgncikgfhdo"

timeoutMs :: Int
timeoutMs = 240_000_000

threeSeconds :: Number
threeSeconds = 3.0

tenSeconds :: Number
tenSeconds = 10.0

specRunnerTimeoutMs :: Number
specRunnerTimeoutMs = 600_000.0

newtype PaymentAddress = PaymentAddress String

derive instance Newtype PaymentAddress _

paymentAddressForTestWallet1 :: PaymentAddress
paymentAddressForTestWallet1 = PaymentAddress "addr_test1qqp9ffn9mesc3gy5hg7a8c2zz8djwj5a24u75t3ms0xfqtfufkwuw83y6fwmwpxuvkftcv4rlu9h3x5csvuseajezw3qghtejp"

paymentAddressForTestWallet2 :: PaymentAddress
paymentAddressForTestWallet2 = PaymentAddress "addr_test1qqxvm2aedwujv6yc8mq3pr530xt3kejgzv6vduea50symvvzlqxmnpk5qkpfdhtf5u00sxspg32adwvcsdmz7w49tm0sfh9ksx"

paymentAddressForTestWallet3 :: PaymentAddress
paymentAddressForTestWallet3 = PaymentAddress "addr_test1qqjq62y800h374h48dn7zwscuugepsadkfh7xh658zxnts3n6j8pzskwwugnyfstukrskwmc9np92lhc0rmr359aqpgswqay7j"
