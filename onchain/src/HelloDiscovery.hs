module HelloDiscovery (
  nftCbor,
) where

import Plutarch.Prelude

import Plutarch.Api.V2

import Plutarch.Extensions.Api (passert)
import Plutarch.Extensions.Data(parseData)
import Utils (closedTermToHexString)

nftCbor :: Maybe String
nftCbor = closedTermToHexString standardNFT

standardNFT :: ClosedTerm (PData :--> PMintingPolicy)
standardNFT = phoistAcyclic $
  plam $ \outRefData _ sc -> unTermCont $ do
    outRef :: Term _ PTxOutRef <- parseData outRefData
    let (inputs :: Term _ (PBuiltinList PTxOutRef)) =
          pmap # pfield @"outRef"
            #$ pfield @"inputs"
            #$ pfield @"txInfo" # sc
    passert "didn't spend out ref" $ pelem # outRef # inputs

