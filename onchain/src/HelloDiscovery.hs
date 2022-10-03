module HelloDiscovery (
  configScriptCbor,
  nftCbor,
  standardNftMp,
  authTokenCbor,
  vaultScriptCbor,
) where

import Plutarch.Prelude

import Plutarch.Api.V2 (
  PAddress,
  PDatum (PDatum),
  PMintingPolicy,
  POutputDatum (..),
  PPubKeyHash,
  PScriptPurpose (PMinting, PSpending),
  PTxInInfo,
  PTxOutRef,
  PValidator,
  mkMintingPolicy,
  mkValidator,
  validatorHash,
 )

import Plutarch.Api.V1 (
  PCredential (PScriptCredential),
  PCurrencySymbol,
  PMap,
  PTokenName (PTokenName),
  PValue (PValue),
 )

import Utils (closedTermToHexString, globalConfig, validatorToHexString)

import PlutusLedgerApi.V2 (MintingPolicy, TxOutRef, adaToken)

import Plutarch.Api.V1.AssocMap qualified as AssocMap
import Plutarch.Api.V1.Value qualified as Value

import Plutarch.Api.V1.Value (pvalueOf)
import Plutarch.Builtin (pforgetData, pserialiseData)
import Plutarch.Crypto (pblake2b_256)
import Plutarch.DataRepr (PDataFields)
import Plutarch.Extensions.Api (passert, passert_, pfindOwnInput, pgetContinuingOutput)
import Plutarch.Extensions.Data (parseData, parseDatum)
import Plutarch.Extensions.List (unsingleton)
import Plutarch.Extensions.Monad (pmatchFieldC)
import Plutarch.Extra.TermCont

configScriptCbor :: String
configScriptCbor = validatorToHexString $ mkValidator globalConfig configScript

nftCbor :: Maybe String
nftCbor = closedTermToHexString standardNft

authTokenCbor :: Maybe String
authTokenCbor = closedTermToHexString authTokenMP

vaultScriptCbor :: Maybe String
vaultScriptCbor = closedTermToHexString vaultAdrValidator

{- | The config validator
 since read only spends don't trigger the validator
 and we don't allow config updates
 this script can just error
-}
configScript :: ClosedTerm PValidator
configScript = perror

{- | The standard NFT minting policy
 parametized by a txid
 to mint:
 the txid must be spent as an input
-}
standardNftMp :: TxOutRef -> MintingPolicy
standardNftMp outRef =
  mkMintingPolicy globalConfig $
    standardNft # pforgetData (pdata (pconstant outRef))

standardNft :: ClosedTerm (PData :--> PMintingPolicy)
standardNft = phoistAcyclic $
  plam $ \outRefData _ sc -> unTermCont $ do
    outRef :: Term _ PTxOutRef <- parseData outRefData
    let (inputs :: Term _ (PBuiltinList PTxOutRef)) =
          pmap # pfield @"outRef"
            #$ pfield @"inputs"
            #$ pfield @"txInfo" # sc
    passert "didn't spend out ref" $ pelem # outRef # inputs

{- | The authorisation+discovery token minting policy
 parametized by the vault address
 to mint:
 redeemer must be of the form Burn | AuthRedeemer tokenName txid
 when it's burn
   no tokens may be minted
 when it's AuthRedeemer tokenName txid
   tokenName must be the hash of the txid
   the txid must be spent in the transaction
   the txid and tn as its hash must be included as a datum in the lookups
   the output at the vault address must be unique
   its value must include the nft
   its datum must parse and the counter must be 0
-}
authTokenMP :: ClosedTerm (PData :--> PMintingPolicy)
authTokenMP = ptrace "vault auth mp" $
  phoistAcyclic $
    plam $ \vaultAdrData redeemerData sc -> unTermCont $ do
      (redeemer :: Term _ AuthRedeemer) <- parseData redeemerData
      info <- pletC $ pfield @"txInfo" # sc
      let (minting :: Term _ (PValue _ _)) = pfield @"mint" # info
      PMinting cs' <- pmatchC (pfield @"purpose" # sc)
      cs :: Term _ PCurrencySymbol <- pletC $ pfield @"_0" # cs'
      mintedAtCs <- pletC $ atCS # minting # cs
      pmatchC redeemer >>= \case
        Burning _ -> do
          passert "was just burning" $
            AssocMap.pall # plam (#< 0) # mintedAtCs
        AuthRedeemer red -> do
          -- misc lookups
          vaultAdr :: Term _ PAddress <- parseData vaultAdrData
          tn <- pletC $ pfield @"tokenName" # red
          ref :: Term _ PTxOutRef <- pletC $ pfield @"txOutRef" # red

          -- Ref is spent
          passert_ "ref is spent" $
            pelem # ref #$ pmap # (pfield @"outRef") #$ pfromData $ pfield @"inputs" # info

          -- Token name is hash of ref
          PTokenName tn' <- pmatchC tn
          passert_ "tn was hash of ref" $
            tn' #== (pblake2b_256 #$ pserialiseData # pforgetData (pdata ref))

          -- mints exactly one token
          passert_ "did not mint exactly one token of this currency symbol" $
            isJustTn # mintedAtCs # tn

          -- Exactly one vault output
          let outputs = pfield @"outputs" # info
          let filtered = pfilter # plam ((vaultAdr #==) . (pfield @"address" #)) # outputs
          vault <- pletC $ unsingleton filtered

          -- NFT sent to vault
          val <- pletC $ pfield @"value" # vault
          passert_ "nft went to vault" $ pvalueOf # val # cs # tn #== 1

          -- Counter starts at 0
          outDatum <- pletC $ pfield @"datum" # vault
          datum' <-
            pmatchC outDatum >>= \case
              POutputDatum d -> pure $ pfield @"outputDatum" # d
              POutputDatumHash _ -> fail "datum hash not supported"
              PNoOutputDatum _ -> fail "no data"
          PDatum dat <- pmatchC datum'
          (datum :: Term _ CounterDatum) <- parseData dat
          passert "count wasn't 0" $ (0 :: Term _ PInteger) #== pfield @"count" # datum

{- | The vault address validator
 paremetized by an asset class
 to validate:
 The owner must sign the tx
 the config must be a read only input
 the utxo must have a valid nft
 the redemer must be the tokenName of the nft and an inc or spend
 if it's inc
 there must be a new output
 it must have the same owner
 the counter must be 1 higher
 the new output must have the same nft
 if it's spend
 the nft must be burned
-}
vaultAdrValidator :: ClosedTerm (PData :--> PValidator)
vaultAdrValidator = ptrace "vaultAdrValidator" $
  plam $ \configNftCsData datum' redeemer' sc -> unTermCont $ do
    datum :: Term _ CounterDatum <- parseData datum'
    info <- pletC $ pfield @"txInfo" # sc
    passert_ "owner signed tx" $
      pelem # (pfield @"owner" # datum) #$ pfield @"signatories" # info
    redeemer :: Term _ HelloRedeemer <- parseData redeemer'
    configNftCs :: Term _ PCurrencySymbol <- parseData configNftCsData
    PJust config <- pmatchC $ pfind # (isConfigInput # configNftCs) # (pfield @"referenceInputs" # info)
    POutputDatum configDatum <- pmatchC (pfield @"datum" #$ pfield @"resolved" # config)
    PDatum configData <- pmatchFieldC @"outputDatum" configDatum
    nftCs <- parseData configData
    PSpending outRef <- pmatchC $ pfield @"purpose" # sc
    PJust inInfo <- pmatchC $ pfindOwnInput # (pfield @"inputs" # info) #$ pfield @"_0" # outRef
    nftTn :: Term _ PTokenName <- pletC $ pfield @"tokenName" # redeemer
    passert_ "has nft" $ hasExactlyThisNft # nftCs # nftTn # (pfield @"value" #$ pfield @"resolved" # inInfo)
    pmatchC (pfield @"action" # redeemer) >>= \case
      Inc _ -> do
        out <- pgetContinuingOutput sc
        POutputDatum outDatum <- pmatchC $ pfield @"datum" # out
        datum2 :: Term _ CounterDatum <- parseDatum (pfield @"outputDatum" # outDatum)
        passert_ "owner is the same" $ pfield @"owner" # datum2 #== pfield @"owner" # datum
        passert_ "count is 1 more" $ pfield @"count" # datum2 #== pfield @"count" # datum + (1 :: Term _ PInteger)
        passert "kept nft" $ hasExactlyThisNft # nftCs # nftTn #$ pfield @"value" #out
      Spend _ -> do
        let minting = pfield @"mint" # info
        passert "burned nft" $ Value.pvalueOf # minting # nftCs # nftTn #< 0

isConfigInput :: ClosedTerm (PCurrencySymbol :--> PTxInInfo :--> PBool)
isConfigInput = phoistAcyclic $
  plam $ \cs inInfo -> unTermCont $ do
    out <- pletC $ pfield @"resolved" # inInfo
    configAdr <- pletC $ pcon (PScriptCredential $ pdcons # pdata (pconstant (validatorHash $ mkValidator globalConfig configScript)) # pdnil)
    let isAtConfigAdr = (pfield @"credential" #$ pfield @"address" # out) #== configAdr
    let hasConfigNft = 0 #< Value.pvalueOf # (pfield @"value" # out) # cs # pconstant adaToken
    pure $ isAtConfigAdr #&& hasConfigNft

hasExactlyThisNft :: ClosedTerm (PCurrencySymbol :--> PTokenName :--> PValue 'Value.Sorted a :--> PBool)
hasExactlyThisNft = phoistAcyclic $ plam $ \cs tn val -> isJustTn # (atCS # val # cs) # tn

isJustTn :: ClosedTerm (PMap 'Value.Sorted PTokenName PInteger :--> PTokenName :--> PBool)
isJustTn = phoistAcyclic $ plam $ \m tn -> m #== AssocMap.psingleton # tn # 1

atCS :: ClosedTerm (PValue s a :--> PCurrencySymbol :--> PMap s PTokenName PInteger)
atCS = phoistAcyclic $
  plam $ \val cs -> unTermCont $ do
    PValue valMap <- pmatchC val
    PJust subMap <- pmatchC $ AssocMap.plookup # cs # valMap
    pure subMap

-- Types

newtype HelloRedeemer (s :: S)
  = HelloRedeemer
      ( Term
          s
          ( PDataRecord
              '[ "tokenName" ':= PTokenName
               , "action" ':= HelloAction
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields, PEq)

instance DerivePlutusType HelloRedeemer where type DPTStrat _ = PlutusTypeData
instance PTryFrom PData (PAsData HelloRedeemer)

instance PTryFrom PData HelloAction

data HelloAction (s :: S)
  = Inc (Term s (PDataRecord '[]))
  | Spend (Term s (PDataRecord '[]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PEq)

instance DerivePlutusType HelloAction where type DPTStrat _ = PlutusTypeData
instance PTryFrom PData (PAsData HelloAction)

newtype CounterDatum (s :: S)
  = CounterDatum
      ( Term
          s
          ( PDataRecord
              '[ "owner" ':= PPubKeyHash
               , "count" ':= PInteger
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields, PEq, PShow)

instance DerivePlutusType CounterDatum where type DPTStrat _ = PlutusTypeData
instance PTryFrom PData (PAsData CounterDatum)

data AuthRedeemer (s :: S)
  = AuthRedeemer
      ( Term
          s
          ( PDataRecord
              '[ "tokenName" ':= PTokenName
               , "txOutRef" ':= PTxOutRef
               ]
          )
      )
  | Burning (Term s (PDataRecord '[]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PEq, PShow)

instance DerivePlutusType AuthRedeemer where type DPTStrat _ = PlutusTypeData
instance PTryFrom PData (PAsData AuthRedeemer)
