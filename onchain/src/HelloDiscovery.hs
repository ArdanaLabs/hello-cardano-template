module HelloDiscovery (
  configScriptCbor,
  nftCbor,
  authTokenCbor,
) where

import Plutarch.Prelude

import Plutarch.Api.V2

import Plutarch.Api.V1.AssocMap qualified as AssocMap
import Plutarch.Api.V1.Value qualified as Value

import Data.Default (Default (def))
import Plutarch.Api.V1 (PTokenName (PTokenName), PValue (PValue), PCurrencySymbol)
import Plutarch.DataRepr (PDataFields)
import Plutarch.Extensions.Api (passert, passert_)
import Plutarch.Extensions.Data (parseData)
import Plutarch.Extensions.List (unsingleton)
import Plutarch.Extra.TermCont
import Utils (closedTermToHexString, validatorToHexString)
import Plutarch.Api.V1.AssocMap (plookup)
import Plutarch.Builtin (pforgetData)
import Plutarch.Api.V1.Value (pforgetPositive)

configScriptCbor :: String
configScriptCbor = validatorToHexString $ mkValidator def configScript

nftCbor :: Maybe String
nftCbor = closedTermToHexString standardNFT

authTokenCbor :: Maybe String
authTokenCbor = closedTermToHexString authTokenMP

-- | The config validator
-- the config validator
-- th tx being spent must be read only
-- TODO does a read only spend even invoke a validator?
-- if not we can just do const False or something
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

-- | The standard NFT minting policy
-- parametized by a txid
-- to mint:
-- the txid must be spent as an input
standardNFT :: ClosedTerm (PData :--> PMintingPolicy)
standardNFT = phoistAcyclic $
  plam $ \outRefData _ sc -> unTermCont $ do
    outRef :: Term _ PTxOutRef <- parseData outRefData
    let (inputs :: Term _ (PBuiltinList PTxOutRef)) =
          pmap # pfield @"outRef"
            #$ pfield @"inputs"
            #$ pfield @"txInfo" # sc
    passert "didn't spend out ref" $ pelem # outRef # inputs

-- | The authorisation+discovery token minting policy
-- parametized by the vault address
-- to mint:
-- redeemer must be of the form AuthRedeemer tokenName txid
-- tn must be the hash of the txid
-- the txid must be spent in the transaction
-- the txid and tn as its hash must be included as a datum in the lookups
-- the output at the vault address must be unique
-- its value must include the nft
-- its datum must parse and the counter must be 0
authTokenMP :: ClosedTerm (PData :--> PMintingPolicy)
authTokenMP = phoistAcyclic $
  plam $ \vaultAdrData redeemerData sc -> unTermCont $ do
    -- misc lookups
    vaultAdr :: Term _ PAddress <- parseData vaultAdrData
    PMinting cs' <- pmatchC (pfield @"purpose" # sc)
    cs :: Term _ PCurrencySymbol  <- pletC $ pfield @"_0" # cs'
    info <- pletC $ pfield @"txInfo" # sc
    (redeemer :: Term _ AuthRedeemer) <- parseData redeemerData
    tn <- pletC $ pfield @"tokenName" # redeemer
    txid :: Term _ PTxId <- pletC $ pfield @"txid" # redeemer

    -- Token name is hash of txid
    PTokenName tn' <- pmatchC tn
    passert_ "tn was hash of txid" $
      plookup # pcon (PDatumHash tn') # (pfield @"datums" # info)
          #== pcon (PJust $ pcon $ PDatum $ pforgetData $ pdata txid)

    -- mints exactly one token
    let minting = pfield @"mint" # info
    PValue m <- pmatchC minting
    PJust mintedAtCs <- pmatchC $ AssocMap.plookup # cs # m
    passert_ "did not mint exactly one token of this currency symbol" $
      mintedAtCs #== (AssocMap.psingleton # tn # 1)

    -- Exactly one vault output
    let outputs = pfield @"outputs" # info
    vault <- pletC $ unsingleton $ pfilter # plam ((vaultAdr #==) . (pfield @"address" #)) # outputs

    -- NFT sent to vault
    val <- pletC $ pfield @"value" # vault
    passert_ "nft went to vault" $ (Value.psingleton # cs # tn # 1) #<= pforgetPositive val

    -- Counter starts at 0
    outDatum <- pletC $ pfield @"datum" # vault
    POutputDatum datum' <- pmatchC outDatum
    PDatum dat <- pmatchC $ pfield @"outputDatum" # datum'
    (datum :: Term _ CounterDatum) <- parseData dat
    passert "count wasn't 0" $ (0 :: Term _ PInteger) #== pfield @"count" # datum

-- Types
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

newtype AuthRedeemer (s :: S)
  = AuthRedeemer
    ( Term
      s
      ( PDataRecord
          '[ "tokenName" ':= PTokenName
           , "txid" ':= PTxId
           ]
      )
    )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields, PEq)
instance DerivePlutusType AuthRedeemer where type DPTStrat _ = PlutusTypeData
instance PTryFrom PData (PAsData AuthRedeemer)
