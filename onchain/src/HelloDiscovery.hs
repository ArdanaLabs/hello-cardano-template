module HelloDiscovery (
  configScriptCbor,
  nftCbor,
  authTokenCbor,
) where

import Plutarch.Prelude

import Plutarch.Api.V2

import Data.Default (Default (def))
import Plutarch.Api.V1 (PTokenName (PTokenName), PValue (PValue))
import Plutarch.Api.V1.AssocMap qualified as AssocMap
import Plutarch.DataRepr (PDataFields)
import Plutarch.Extensions.Api (passert, passert_)
import Plutarch.Extensions.Data (parseData)
import Plutarch.Extensions.List (unsingleton)
import Plutarch.Extra.TermCont
import Utils (closedTermToHexString, validatorToHexString)

newtype CounterDatum (s :: S)
  = CounterDatum
      ( Term
          s
          ( PDataRecord
              '[ "owner" ':= PAddress
               , "count" ':= PInteger
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields, PEq)

instance DerivePlutusType CounterDatum where type DPTStrat _ = PlutusTypeData
instance PTryFrom PData (PAsData CounterDatum)

configScriptCbor :: String
configScriptCbor = validatorToHexString $ mkValidator def configScript

nftCbor :: Maybe String
nftCbor = closedTermToHexString standardNFT

authTokenCbor :: Maybe String
authTokenCbor = closedTermToHexString authTokenMP

configScript :: ClosedTerm PValidator
configScript = phoistAcyclic $
  plam $ \_datum _redemer sc -> unTermCont $ do
    PSpending outRef' <- pmatchC (pfield @"purpose" # sc)
    let (outRef :: Term _ PTxOutRef) =
          pfield @"_0" # outRef'
        (refrenceInputOutRefs :: Term _ (PBuiltinList PTxOutRef)) =
          pmap # pfield @"outRef"
            #$ pfield @"referenceInputs"
            #$ pfield @"txInfo" # sc
    passert "wasn't a refrence input" $ pelem # outRef # refrenceInputOutRefs

standardNFT :: ClosedTerm (PData :--> PMintingPolicy)
standardNFT = phoistAcyclic $
  plam $ \outRefData _ sc -> unTermCont $ do
    outRef :: Term _ PTxOutRef <- parseData outRefData
    let (inputs :: Term _ (PBuiltinList PTxOutRef)) =
          pmap # pfield @"outRef"
            #$ pfield @"inputs"
            #$ pfield @"txInfo" # sc
    passert "didn't spend out ref" $ pelem # outRef # inputs

authTokenMP :: ClosedTerm (PData :--> PMintingPolicy)
authTokenMP = phoistAcyclic $
  plam $ \vaultAdrData redeemer sc -> unTermCont $ do
    vaultAdr :: Term _ PAddress <- parseData vaultAdrData
    PMinting cs' <- pmatchC (pfield @"purpose" # sc)
    cs <- pletC $ pfield @"_0" # cs'
    info <- pletC $ pfield @"txInfo" # sc
    let minting = pfield @"mint" # info
    PValue m <- pmatchC minting
    PJust mintedAtCs <- pmatchC $ AssocMap.plookup # cs # m
    tn <- parseData redeemer
    passert_ "did not mint exactly one token of this currency symbol" $
      mintedAtCs #== (AssocMap.psingleton # tn # 1)
    let outputs = pfield @"outputs" # info
    vault <- pletC $ unsingleton $ pfilter # plam ((vaultAdr #==) . (pfield @"address" #)) # outputs
    outDatum <- pletC $ pfield @"datum" # vault
    POutputDatum datum' <- pmatchC outDatum
    PDatum dat <- pmatchC $ pfield @"outputDatum" # datum'
    (datum :: Term _ CounterDatum) <- parseData dat
    passert_ "count wasn't 0" $ (0 :: Term _ PInteger) #== pfield @"count" # datum
    PTokenName tn' <- pmatchC tn
    tnid <- pletC $ psliceBS # 0 # 27 # tn'
    tnidx <- pletC $ psliceBS # 28 # 31 # tn'
    let (inputs :: Term _ (PBuiltinList PTxInInfo)) = pfield @"inputs" # info
    passert "spent right input for token name" $
      pany
        # plam
          ( \input -> unTermCont $ do
              out <- pletC $ pfield @"outRef" # input
              pure $
                (pfield @"_0" #$ pfield @"id" # out) #== tnid
                  -- we could allow more than one byte for the idx but I don't think we need to
                  #&& (pconsBS # (pfield @"idx" # out) # pconstant "000" #== tnidx)
          )
        #$ inputs
