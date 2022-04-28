{-# LANGUAGE TemplateHaskell #-}

module HelloWorld.ValidatorUtils (getValidatorScriptsPath) where

import Language.Haskell.TH.Env (envQ)

getValidatorScriptsPath :: String
getValidatorScriptsPath = maybe (error "environment variable not found") id ($$(envQ "DUSD_SCRIPTS") :: Maybe String)
