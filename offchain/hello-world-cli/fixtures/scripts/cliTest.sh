#!/bin/sh

nix build .#hello-world-cli

PATH=$PATH:$PWD/result/bin

CONF=../jsons/testWalletCfg.json

#lock

#no config
hello-world-cli -s script.clistate lock -i 0 -p 1                           && exit -1
# no state
hello-world-cli -c $CONF lock -i 0 -p 1  			                              &&  exit -1
# bad config path
hello-world-cli -c bad_path -s script.clistate lock -i 0 -p 1               &&  exit -1
# bad wallet config
hello-world-cli -c ../jsons/badWalletCfg -s script.clistate lock -i 0 -p 1  &&  exit -1
# state file exists
hello-world-cli -c $CONF -s ../jsons/badState lock -i 0 -p 1                &&  exit -1


#increment

#no config
hello-world-cli -s script.clistate increment            &&  exit -1
#no state
hello-world-cli -c $CONF increment                      &&  exit -1
#bad state path
hello-world-cli -c $CONF -s bad_path increment          &&  exit -1
# bad state file
hello-world-cli -c $CONF -s ../jsons/badState increment &&  exit -1

#unlock
#no config
hello-world-cli -s script.clistate unlock            &&  exit -1
#no state
hello-world-cli -c $CONF unlock                      &&  exit -1
#bad state path
hello-world-cli -c $CONF -s bad_path unlock          &&  exit -1
# bad state file
hello-world-cli -c $CONF -s ../jsons/badState unlock &&  exit -1

#query
hello-world-cli -s script.clistate query  &&  exit -1

#need to pass
#hello-world-cli --help | grep hello-world-cli               ||  exit -1


#integration test
hello-world-cli -c $CONF -s script.clistate lock -i 0 -p 1  || exit -1
# state file then exists
ls script.clistate                                          || exit -1
hello-world-cli -c $CONF -s script.clistate query           || exit -1
hello-world-cli -c $CONF -s script.clistate increment       || exit -1
hello-world-cli -c $CONF -s script.clistate query           || exit -1
hello-world-cli -c $CONF -s script.clistate increment       || exit -1
hello-world-cli -c $CONF -s script.clistate query           || exit -1
hello-world-cli -c $CONF -s script.clistate unlock          || exit -1
# state file no longer exists
ls script.clistate                                          && exit -1

echo cliTest.sh finished
