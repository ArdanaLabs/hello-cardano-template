## How to build and run

To obtain a repl:
```
nix develop .#hello-world-browser
purs-nix srcs repl
```

To build the bundle:
```
nix build .#hello-world-browser
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
nix run .#serve-hello-world-browser
```

To run the api tests (currently just cbor encoding tests):
```
nix develop .#hello-world-api
cd offchain-ctl/hello-world-api
purs-nix test Main
```

To run the browser integration tests
```
nix build -L .#hello-world-browser-test:test:integration
```

the test output will be there in `result` folder, it could be viewed in color with `less -r result/test-stdout`


# Test-wallet

The test wallet addres is:
`addr_test1qqevfdhu80jsmjhzkf8lkkv5rza9h6k0u6hmwwr0r7vyjt9j3f374a2all0hc6vzxa6v7sax7du2lk5fu5q592d5fhqswar4hc`.
Here's a link to faucet: https://testnets.cardano.org/en/testnets/cardano/tools/faucet/ .
If the wallet runs out tests can fail.
