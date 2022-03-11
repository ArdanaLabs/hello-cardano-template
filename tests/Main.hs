module Main ( main ) where
import qualified Apropos.Plutus.SingletonValue as SingletonValue
import qualified Apropos.Plutus.Value as Value
--import Apropos.Plutus.List ( ListModel )
--import Apropos.Plutus.SingletonValue ( SingletonValueProp )
import Test.Tasty ( defaultMain, testGroup )
import Main.Utf8 (withUtf8)
--import Apropos ( LogicalModel(logic), Enumerable(enumerated) )
--import Apropos.LogicalModel ( solveAll )
--import qualified Data.Map as M

-- TODO should we use sydtest-discover?
main :: IO ()
main = withUtf8 $ do -- TODO this seems to break the histograms a bit
  --print (enumerated @(ListModel SingletonValueProp))
  --print (length $ solveAll (logic @SingletonValueProp))
  --mapM_ (\(i,s) -> print i >> print (M.filter id s)) $ zip [1 :: Int ..] (solveAll (logic @(ListModel SingletonValueProp)))
  --countLen 100 $ solveAll (logic @(ListModel SingletonValueProp))
  defaultMain $ testGroup "Plutus"
    [ SingletonValue.singletonValueGenSelfTests
    , Value.valueGenSelfTests
                ]

  {-
countLen :: Int -> [a] -> IO ()
countLen step = go 0
  where
    go n [] = print n
    go n xs = print n >> go (n+step) (drop step xs)
    -}
