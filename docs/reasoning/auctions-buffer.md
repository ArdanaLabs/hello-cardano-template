# Will the buffer split off a UTXO to execute an auction, or will the auctions happen within the buffer?

## Buffer splits off a UTXO to execute an auction

### Pros

- The buffer will not be spent on every auction transaction.
- If there’s a bug in the auction’s on-chain code, “only” the auctioned amount can be drained/stolen, not the entire buffer.
- Auction code would be much more robust, easier to write.

### Cons

- The UTXO is outside of buffer application logic so it cannot be synchronously invalidated.
Auction sizing doesn’t really work, because you can’t prove on-chain there are no auctions launched already, so anyone can launch multiple auctions. This means auction UTXOs can’t be split off, there would have to be one “persistent” auction UTXO.

## Auctions happen within the buffer

### Pros

- Control flow is single threaded so auction logic could be more directly tied to the resources the buffer has available to it.

### Cons

- More resource contention. If auctions are consuming the buffer at the same time as other transactions, a user may need to keep constructing their transaction over and over until the UTXO they’re referencing is unspent.

## Conclusion

Auctions happen within the buffer.

