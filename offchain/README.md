# How to build and run

## Test-bundles

To run all offchain tests with plutip use
```
nix run .#offchain:test:local
```
to run all offchain tests with testnet run
```
nix run .#offchain:test:testnet
```
(this requires ctl-runtime)

## Ctl-runtime

In order for some tests and apps to work you need to be running the Ctl-runtime.
The ctl-runtime requires that docker is installed and the docker daemon is running.
Installing docker will depend on your package manager, but might look like `sudo apt-get docker` or `pacman -S docker`.
Starting the docker daemon depends on your init system but is usually `sudo systemctl start docker`.
You probably also want to run `sudo systemctl enable docker` so that docker will start automatically when your reboot.

Once you have docker you can start the ctl-runtime by running:

```
nix run .#ctl-runtime
```

Currently only the browser tests require the ctl-runtime.
The cli and api tests use plutip.

## The API

The api also has a dev shell which can be entered with `nix develop .#offchain:hello-world-api`.
From the dev shell you can:
- run tests with `purs-nix test` (after setting the mode `export MODE=local` or `export MODE=testnet`)
- obtain a repl with `purs-nix repl`
- compile the api with `purs-nix compile` This will not compile tests

To run the api tests with plutip run:
```
nix run .#offchain:hello-world-api:test:local
```
or with testnet run:
```
nix run .#offchain:hello-world-api:test:testnet
```

## The Browser-app

To enter the browser app's dev shell run `nix develop .#offchain:hello-world-browser`.

If you want to use the browser app make sure you with a current testnet version of the Nami wallet
browser extension which can be found
in the chrom webstore [here](https://chrome.google.com/webstore/detail/nami/lpfcbjknijpeeillifnkikgncikgfhdo)
or from their github [here](https://github.com/berry-pool/nami).
Currently this requires a browser that can run Chrome extensions.
The browser tests also seem to have issues in older versions of chromium,
Chromium 102.0.5005.61 is known to work.

Once you are in the dev shell you can obtain a repl with: `purs-nix repl`, or compile the browser app with `purs-nix compile`.

To run the browser app you can use:
```
nix run .#offchain:hello-world-browser:serve
```

To run the browser tests with testnet use:
```
nix run .#offchain:hello-world-browser:test:testnet
```

To run the browser tests with plutip use:
```
nix run .#offchain:hello-world-browser:test:local
```

To run the lighthouse tests use:
```
nix build .#checks.x86_64-linux."offchain:hello-world-browser:lighthouse"
```
then view the generated report (result/lighthouse-report.json) with https://googlechrome.github.io/lighthouse/viewer/

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

To run the CLI tests, with plutip run:
```
nix run .#offchain:hello-world-cli:test:local
```
or with testnet run
```
nix run .#offchain:hello-world-cli:test:testnet
```
or in the dev shell run
```
nix build .#offchain:hello-world-cli
export PATH=$PATH:./result/bin
export MODE=local # or testnet if you want testnet
purs-nix test
```

## wallet configs

The wallet config is a json file who's path must be provided in the `CONFIG_FILE` argument of each cli command.
The config format has the following fields:
- `walletPath` which needs to be the path to a skey file for the wallet
- Optional `stakingPath` which needs to be the path to a skey file for the staking credential, if omitted the wallet will not have a staking key.
- network which needs to be `Testnet` or `Mainnet` and must indicate which network the wallet belongs to and the test will use.

Here's an example config:
```
{ "walletPath" : "wallet.skey"
, "stakingPath" : "staking.skey"
, "network" : "Testnet"
}
```
