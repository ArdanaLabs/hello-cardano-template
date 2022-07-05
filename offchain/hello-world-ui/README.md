## Start the the local dev server

```console
$ nix run .#hello-world-ui
```

then open URL http://127.0.0.1:8000 in the browser

## Enter a development shell with all the required toolchain

```console
$ nix develop .#hello-world-ui
```

## Build the project with Nix

```console
$ nix build .#hello-world-ui
```

