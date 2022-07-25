## How to build and run

To obtain a repl:
```
nix develop .#offchain:hello-world-browser
purs-nix srcs repl
```

To build the bundle:
```
nix build .#offchain:hello-world-browser
```

To try out the app after building do:
```
NIXPKGS_ALLOW_INSECURE=1 nix run --impure .#apps.x86_64-linux.ctl-runtime
```
and afterwards either:
```
nix shell nixpkgs#nodePackages.http-server
http-server -c-1 result
```
or
```
nix run .#offchain:hello-world-browser:serve
```

To run the api tests (currently just cbor encoding tests):
```
nix run .#offchain:hello-world-api:test
```
or
```
nix develop .#offchain:hello-world-api
cd offchain/hello-world-api
purs-nix test Main
```

To run the browser integration tests
```
nix run .#offchain:hello-world-browser:test
```
the test output will be outputted to stdout, or
```
nix build -L .#hello-world-browser-test:test:integration
```

the test output will be there in `result` folder, it could be viewed in color with `less -r result/test-stdout`

# cli

The cli can be used either by running
```
nix run .#hello-world-cli -- <args>
```
, from the dev shell with
```
nix develop .#hello-world-cli
# and then
purs-nix run <args>
```
or building it and using the result with
```
nix build .#hello-world-cli
# and then
./result/bin/hello-world-cli <args>
```

## wallet configs

The wallet config is a json file who's path must be provided in the `CONFIG_FILE` argument of each cli command.
It must provide the fields:
- `walletPath` which needs to be the path to a skey file for the wallet
- `stakingPath` which needs to be the path to a skey file for the staking credential
- network which needs to be `Testnet` or `Mainnet` and must indicate which network the wallet belongs to and the test will use.

Here's an example config:
```
{ "walletPath" : "wallet.skey"
, "stakingPath" : "staking.skey"
, "network" : "Testnet"
}
```

## cli tests

TODO OUTDATED

To run the CLI tests, run:
```
nix run .#offchain:hello-world-cli:test
```
or use `purs-nix test` in the `.#hello-world-cli` dev shell.

# Test-wallet

The test wallet addres is:
`addr_test1qqevfdhu80jsmjhzkf8lkkv5rza9h6k0u6hmwwr0r7vyjt9j3f374a2all0hc6vzxa6v7sax7du2lk5fu5q592d5fhqswar4hc`.
Here's a link to faucet: https://testnets.cardano.org/en/testnets/cardano/tools/faucet/ .
If the wallet runs out tests can fail.

