#!/bin/sh
purs-nix run -c testWalletCfg.json -s script.clistate lock -i 0 -p 1  || exit -1
purs-nix run -c testWalletCfg.json -s script.clistate querry          || exit -1
purs-nix run -c testWalletCfg.json -s script.clistate inc             || exit -1
purs-nix run -c testWalletCfg.json -s script.clistate querry          || exit -1
purs-nix run -c testWalletCfg.json -s script.clistate inc             || exit -1
purs-nix run -c testWalletCfg.json -s script.clistate querry          || exit -1
purs-nix run -c testWalletCfg.json -s script.clistate end             || exit -1

nix build .#hello-world-cli

./result/bin/hello-world-cli -c testWalletCfg.json -s script.clistate lock -i 0 -p 1  || exit -1
./result/bin/hello-world-cli -c testWalletCfg.json -s script.clistate querry          || exit -1
./result/bin/hello-world-cli -c testWalletCfg.json -s script.clistate inc             || exit -1
./result/bin/hello-world-cli -c testWalletCfg.json -s script.clistate querry          || exit -1
./result/bin/hello-world-cli -c testWalletCfg.json -s script.clistate inc             || exit -1
./result/bin/hello-world-cli -c testWalletCfg.json -s script.clistate querry          || exit -1
./result/bin/hello-world-cli -c testWalletCfg.json -s script.clistate end             || exit -1
