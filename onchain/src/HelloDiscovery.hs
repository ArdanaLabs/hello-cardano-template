module HelloDiscovery (
  nftCbor,
  standardNftMp,
) where

import Plutarch.Prelude

import Plutarch.Api.V2

import Data.Default (Default (def))
import Plutarch.Extensions.Api (passert)
import Plutarch.Extensions.Data (parseData)
import PlutusLedgerApi.V2 (MintingPolicy, TxOutRef, toData)
import Utils (closedTermToHexString)

nftCbor :: Maybe String
nftCbor = closedTermToHexString standardNFT

standardNftMp :: TxOutRef -> MintingPolicy
standardNftMp outRef = mkMintingPolicy def (standardNFT # pconstant (toData outRef))

standardNFT :: ClosedTerm (PData :--> PMintingPolicy)
standardNFT = phoistAcyclic $
  plam $ \outRefData _ sc -> unTermCont $ do
    outRef :: Term _ PTxOutRef <- parseData outRefData
    let (inputs :: Term _ (PBuiltinList PTxOutRef)) =
          pmap # pfield @"outRef"
            #$ pfield @"inputs"
            #$ pfield @"txInfo" # sc
    passert "didn't spend out ref" $ pelem # outRef # inputs
