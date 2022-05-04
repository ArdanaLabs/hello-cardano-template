{-# LANGUAGE TemplateHaskell #-}

module HelloWorld.ValidatorUtils (getValidatorScriptsPath) where

import Data.Maybe (fromMaybe)
import Language.Haskell.TH.Env (envQ)

getValidatorScriptsPath :: String
getValidatorScriptsPath = fromMaybe (error "environment variable not found") ($$(envQ "DUSD_SCRIPTS") :: Maybe String)
