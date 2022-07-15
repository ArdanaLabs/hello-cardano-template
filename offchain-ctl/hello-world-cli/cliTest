#!/bin/sh
purs-nix run -c testWalletCfg -s script.clistate lock -i 0 -p 1  || exit -1
purs-nix run -c testWalletCfg -s script.clistate querry          || exit -1
purs-nix run -c testWalletCfg -s script.clistate inc             || exit -1
purs-nix run -c testWalletCfg -s script.clistate querry          || exit -1
purs-nix run -c testWalletCfg -s script.clistate inc             || exit -1
purs-nix run -c testWalletCfg -s script.clistate querry          || exit -1
purs-nix run -c testWalletCfg -s script.clistate end             || exit -1

nix run .#hello-world-cli -- -c testWalletCfg -s script.clistate lock -i 0 -p 1  || exit -1
nix run .#hello-world-cli -- -c testWalletCfg -s script.clistate querry          || exit -1
nix run .#hello-world-cli -- -c testWalletCfg -s script.clistate inc             || exit -1
nix run .#hello-world-cli -- -c testWalletCfg -s script.clistate querry          || exit -1
nix run .#hello-world-cli -- -c testWalletCfg -s script.clistate inc             || exit -1
nix run .#hello-world-cli -- -c testWalletCfg -s script.clistate querry          || exit -1
nix run .#hello-world-cli -- -c testWalletCfg -s script.clistate end             || exit -1
