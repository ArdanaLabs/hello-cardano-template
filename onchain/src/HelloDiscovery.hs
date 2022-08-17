module HelloDiscovery (
  configScriptCbor,
) where

import Plutarch.Prelude

import Data.Default (Default (def))
import Plutarch.Api.V2
import Plutarch.Extensions.Api (passert)
import Plutarch.Extra.TermCont
import Utils (validatorToHexString)

configScriptCbor :: String
configScriptCbor = validatorToHexString $ mkValidator def configScript

configScript :: ClosedTerm PValidator
configScript = phoistAcyclic $
  plam $ \_datum _redemer sc -> unTermCont $ do
    pmatchC (pfield @"purpose" # sc) >>= \case
      PSpending outRef' -> do
        let (outRef :: Term _ PTxOutRef) = pfield @"_0" # outRef'
            (refrenceInputs :: Term _ (PBuiltinList PTxInInfo)) = pfield @"referenceInputs" # (pfield @"txInfo" # sc :: Term _ PTxInfo)
            (refrenceInputOutRefs :: Term _ (PBuiltinList PTxOutRef)) = pmap # pfield @"outRef" # refrenceInputs
        passert "wasn't a refrence input" $ pelem # outRef # refrenceInputOutRefs
      _ ->
        pure $ ptraceError "not a spending tx"
