{- | Executable that runs a cluster with the Hello World PAB.
 This exists primarily for testing i.e. verifying that the cluster
 + PAB launches correctly.
-}
module Main (main) where

import HelloWorld.LocalCluster (runCluster)

main :: IO ()
main = runCluster
