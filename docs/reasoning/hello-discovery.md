# Hello world + Discovery + Validation

The goal of this protocol is to implement the solutions for vault discovery and vault validation into the hello world protocol.
To do this we will add some of the features of stablecoin vaults to the hello world counter utxos here after referred to as vaults.

## The Discovery problem

Users should be able to:
- Get a list of all open vaults in the protocol
- Get a list of all their open vaults
- Inspect the history of a vault

## The Validation problem

It should be possible to enforce rules about how and when vaults can be opened.

# Core solution

For vault discovery the natural solution is one NFT per vault which can be used to track that vault.

For validation the solution is similar.
Part of the design of Cardano is that scripts themselves can't enforce rules about utxos sent to them.
The standard work around is to consider utxos at an address invalid unless they have a token signifying their validity.
The minting policy of the token can then enforce any desired constraints.

In our case it's possible for these two tokens to be the same.
Each token has an asset class which consists of a currency symbol and a token name.
The currency symbol is the hash of the minting policy and the token name is just an arbitrary string.
Because of this a single minting policy can define a family of NFTs where each NFT has a different token name.
In our case the minting policy also enforces that these NFTs can only be minted when opening a valid vault, and must be sent to the vault.
The vault address script validator can further enforce that these NFTs can never leave their vaults.
This is important because otherwise an NFT could be sent instead of minted circumventing the validation.

The need for coordination between the discovery/validation minting policy and the vault address script validator creates an additional issue.
The vault address validator needs to know the currency symbol of the discovery/validation NFTs
but the minting policy needs to know the vault address.
Since the currency symbol is the hash of the minting policy and the address is the hash of the vault script validator this seems to require finding a hash fix-point.

```
Minting policy -hash-> Currency symbol -parameter-> vault script -hash-> vault address -parameter-> Minting policy
```

To solve this problem the vault address script validator can,
instead of taking the currency symbol as a parameter directly,
take an NFT and address which it can then use to locate a config utxo
who's datum will provide the currency symbol.
The config NFT is required to prevent the use of a malicious utxo with a different currency symbol.
The config address script also needs to prevent tampering with the utxo.

# Implementation details

## On-chain components

### Config NFT minting policy

A standard NFT minting policy used for the config NFT (the NFT used to locate the config utxo).
Parameterized by a txid.

### Config Address Script Validator

A validator which enforces that the datum is not changed, and the NFT is not taken.
Ideally this should use read only inputs.

### Vault Validation NFT Minting Policy

A minting policy used to mint NFTs to validation vaults.
Parameterized by the vault Address.
The policy enforces the following constraints:

When minting:

- The token is being minted to the vault Address
- The datum of the vault it's being minted to parses
- The vault's owner signed the transaction
- The vault's count is 0
- Exactly one token is minted

The token can always be burned.

### Vault Address Script Validator

This validator is the core of the protocol.
Parameterized by the config Address and config NFT.
The vault Validation NFT currency symbol is read from the config utxo.

It enforces the following constraints:

- Any spent vault must have a vault Validation NFT
- The owner must sign the transaction

When the increment redeemer is called:

- The datum must be increased by 1
- The owner must not be changed
- The NFT stays with the vault

When the redeem redeemer is called:Closing a vault only needs to enforce that the NFT is burned.

- The NFT must be burned

## Initialization Procedure

1) Create arbitrary Tx by sending a few ada from a wallet to itself
2) Parameterize config NFT with that txid and mint the config NFT.
3) Compute the vault address with the config NFT and Address
4) Compute the vault Validation currency symbol with the vault address
5) Create the config utxo:
	- The address is the config address
	- The value includes the config NFT
	- The datum is the vault Validation currency symbol.

## Protocol actions

Once the protocol is initialized several actions should be possible.

### Open a vault

A vault opening Tx must
- Have an output at the vault address which must
	- have a datum where:
		- the owner is a signatory of the Tx
		- the counter is 0
	- Include an validation NFT in its value.
- The Validation NFT must be minted in the same TX.

### Increment a vault

A vault incrementing Tx must:
- Input a valid vault
- Output a vault with the same owner and NFT and who's count is increased by 1
- Be signed by the owner

### Close a vault

When closing a vault the NFT must be burned.

### Querying Vaults

The api should provide enpoints to query all vaults or all vaults owned by a pubkey.

To query all vaults just query all utxos at the vault address and filter out vaults that don't have an validation token.
To query vaults owned by a particular user further filter by parsing the vault datum and checking the owner.

