# Will the protocol-wide parameters and the buffer (which stores the reserves) be 1 UTXO, or separate ones?

## 1 UTXO

### Pros

- Buffer Admin will only have to update 1 UTXO.

- Transactions referencing the buffer and the system params will only need one reference, so having both in the same UTXO reduces the number UTXOs needed to be referenced when constructing transactions.

### Cons

- The buffer will be updated more frequently than the system params, so participants constructing transactions that only reference system params and not the buffer will also need to update their input reference. This can lead to resource contention, even if the buffer+params UTXO will mostly be used read-only. (This is a big enough problem to veto the option buffer+params+auctions.)

- The UTXO is larger, so there is extra cost.

## Separate UTXOs

### Pros

- Separation of concerns. Let each UTXO represent one purpose (morally).
The system params will be used mostly as a read-only input, which reduces resource contention. Mixing it with the buffer takes this advantage away.

### Cons

 - More complex transactions: Transactions involving the buffer require the system params as a read-only input, instead of having this information accessible directly in the same UTXO

## Conclusion

Let’s split the buffer and system params into separate UTXOs.
