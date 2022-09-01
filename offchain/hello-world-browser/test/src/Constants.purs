module HelloWorld.Test.Constants where

import Contract.Test.E2E (WalletPassword)
import Data.Newtype (class Newtype, wrap)
import Node.Express.Types (Port)

port :: Port
port = 6789

localhost :: String
localhost = "http://127.0.0.1"

namiWalletPassword :: WalletPassword
namiWalletPassword = wrap "ctlctlctl"

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
paymentAddressForTestWallet1 = PaymentAddress "addr_test1qrgzwhfw2w63m7up7swqpcg5r52c05sgwatn4697kgle009akzxrh366l0rqsvjm3q9pyd9fm6t4cegfm286r5lvwh6sa94d5d"

paymentAddressForTestWallet2 :: PaymentAddress
paymentAddressForTestWallet2 = PaymentAddress "addr_test1qp7h5urcrtq67nl2tcz473lzfw29kpl7kn5xxmyxm6yjxk0u6prmwvzm4s48m6v5u4fyfcddaxz27g96880rh03248zsy38gnr"

paymentAddressForTestWallet3 :: PaymentAddress
paymentAddressForTestWallet3 = PaymentAddress "addr_test1qpe8yzu8g9pdqq6xxyv2gm3rn39fwcp44pnx6mmng7nqhz3apa7n8svjgrag8vjn9v5juf2wgzfzh0wzxyezlachfx2san47c2"

faucetUrl :: String
faucetUrl = "https://faucet.cardano-testnet.iohkdev.io/send-money/"

faucetApiKey :: String
faucetApiKey = "r8m9YXmqCkFWDDZ2540IJaJwr1JBxqXB"
