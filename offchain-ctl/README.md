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