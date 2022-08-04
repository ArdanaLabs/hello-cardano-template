# Hello world + Discovery

## Components

### Config NFT minting policy

A standard NFT minting policy used for the config NFT.
Parameterized by a txid.

### Config Address Script Validator

A validator which enforces that the datum is not changed.
Ideally this should use read only inputs.

### Vault Authentication NFT Minting Policy

A minting policy used to mint NFTs to authenticate vaults.
Parameterized by the vault Address.
The policy enforces the following constraints:

When minting:

- The token is being minted to the vault Address
- The vault it's being minted to parses
- The vault's owner signed the transaction
- The vault's count is 0
- Exactly one token is minted

The token can always be burned.

### Vault Address Script Validator

This validator is the core of the protocol.
Parameterized by the config Address and config NFT.
The vault Authentication NFT currency symbol is read from the config utxo.

It enforces the following constraints:

- Any spent vault must have a vault Authentication NFT
- The owner must sign the transaction

When the increment redeemer is called:

- The datum must be increased by 1
- The owner must not be changed
- The NFT stays with the vault

When the redeem redeemer is called:

- The NFT must be burned

## Initialization Procedure

1) Create arbitrary Tx by sending a few ada from a wallet to itself
2) Parameterize config NFT with that txid and mint the config NFT.
3) Compute the vault address with the config NFT and Address
4) Compute the vault Authentication currency symbol with the vault address
5) Create the config utxo. The value includes the config NFT the address is the config address and the datum is the vault Authentication currency symbol.

## Protocol actions

Once the protocol is initialized several actions should be possible.

### Open a vault

A vault opening Tx must
- Have an output at the vault address which must
	- have a datum where:
		- the owner signed the Tx
		- the counter is 0
	- Include an authentication NFT in its value.
- The authentication NFT must be minted in the same TX.

### Increment a vault

A vault incrementing Tx must:
- Input a valid vault
- Output a vault with the same owner and NFT and who's count is increased by 1
- Be signed by the owner

### Close a vault

Closing a vault only needs to enforce that the NFT is burned.

