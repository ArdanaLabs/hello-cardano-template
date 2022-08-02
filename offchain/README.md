# How to build and run

## Ctl-runtime

In order for most tests and apps to work you need to be running the Ctl-runtime.
The ctl-runtime requires that docker is installed and the docker daemon is running.
Installing docker will depend on your package manager, but might look like `sudo apt-get docker` or `pacman -S docker`.
Starting the docker daemon depends on your init system but is usally `sudo systemctl start docker`.
You probably also want to run `sudo systemctl enable docker` so that docker will start automatically when your reboot.

Once you have docker you can start the ctl-runtime by running:

```
nix run .#ctl-runtime
```

## The API

The api also has a dev shell which can be entered with `nix develop .#offchain:hello-world-api`.
From the dev shell you can run tests with `purs-nix test` obtain a repl with `purs-nix repl` or compile the api with `purs-nix compile`.

To run the api tests without the devshell run:
```
nix run .#offchain:hello-world-api:test
```

## The Browser-app

To enter the browser app's dev shell run `nix develop .#offchain:hello-world-browser`.

Once you are in the dev shell you can obtain a repl with: `purs-nix repl`, or compile the browser app with `purs-nix compile`.

To run the browser app you can use:
```
nix run .#offchain:hello-world-browser:serve
```

To run the browser tests use:
```
nix run .#offchain:hello-world-browser:test
```

# The CLI

The CLI also has a dev shell entered with `nix develop .#offchain:hello-world-cli`

The cli can be used either by running
```
nix run .#hello-world-cli -- <args>
```
, from the dev shell with
```
purs-nix run <args>
```
or building it and using the result with
```
nix build .#hello-world-cli
# and then
./result/bin/hello-world-cli <args>
```

To run the CLI tests, run:
```
nix run .#offchain:hello-world-cli:test
```
or use `purs-nix test` in the dev shell.

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

# Test-wallet

Many of our tests use a keywallet on testnet.
The test-wallet addres is:
`addr_test1qqevfdhu80jsmjhzkf8lkkv5rza9h6k0u6hmwwr0r7vyjt9j3f374a2all0hc6vzxa6v7sax7du2lk5fu5q592d5fhqswar4hc`.
Here's a link to faucet: https://testnets.cardano.org/en/testnets/cardano/tools/faucet/ .
If the wallet runs out tests can fail.

