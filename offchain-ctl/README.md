## How to build and run

To build the bundle run:
```
NIXPKGS_ALLOW_INSECURE=1 nix build --impure .#packages.x86_64-linux.hello-world-api
```

To try out the app after building do:
```
NIXPKGS_ALLOW_INSECURE=1 nix run --impure .#apps.x86_64-linux.ctl-scaffold-runtime
nix shell nixpkgs#nodePackages.http-server

http-server -c-1 result/dist/
```