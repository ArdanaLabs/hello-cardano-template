#!/bin/sh

nix build .#hello-world-cli

PATH=$PATH:$PWD/result/bin

CONF=../test-data/testWalletCfg.json

hello-world-cli -c $CONF -s script.clistate lock -i 0 -p 1  ||  exit -1
hello-world-cli -c $CONF -s script.clistate query           ||  exit -1
hello-world-cli -c $CONF -s script.clistate inc             ||  exit -1
hello-world-cli -c $CONF -s script.clistate query           ||  exit -1
hello-world-cli -c $CONF -s script.clistate inc             ||  exit -1
hello-world-cli -c $CONF -s script.clistate query           ||  exit -1
hello-world-cli -c $CONF -s script.clistate end             ||  exit -1

echo cliTest.sh finished
