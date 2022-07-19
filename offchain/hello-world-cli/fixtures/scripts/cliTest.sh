#!/bin/sh

nix build .#hello-world-cli

PATH=$PATH:$PWD/result/bin

CONF=../jsons/testWalletCfg.json

hello-world-cli -c $CONF -s script.clistate lock -i 0 -p 1  ||  exit -1
hello-world-cli -c $CONF -s script.clistate query           ||  exit -1
hello-world-cli -c $CONF -s script.clistate increment       ||  exit -1
hello-world-cli -c $CONF -s script.clistate query           ||  exit -1
hello-world-cli -c $CONF -s script.clistate increment       ||  exit -1
hello-world-cli -c $CONF -s script.clistate query           ||  exit -1
hello-world-cli -c $CONF -s script.clistate unlock          ||  exit -1

echo cliTest.sh finished
