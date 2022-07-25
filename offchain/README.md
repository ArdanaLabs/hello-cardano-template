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

To run the CLI tests, run:
```
nix run .#offchain:hello-world-cli:test
```
