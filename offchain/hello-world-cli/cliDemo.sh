#!/bin/sh

declare_wait_run(){
	echo press enter to run:
	echo $@
	read $_var
	$@
	echo
	echo
	echo
	echo
}


if [[ purs == $1 ]]
then
	echo runing purs-nix compile
	purs-nix compile
	cmd="purs-nix run"
else
	echo runing nix build
	nix build .\#hello-world-cli
	cmd="./result/bin/hello-world-cli"
fi

echo
echo Error examples
echo

echo example: no config arg
declare_wait_run $cmd -s script.clistate lock -i 0 -p 1

echo example: no -i for lock
declare_wait_run $cmd -c testWalletCfg -s script.clistate lock -p 1

echo example: no config file example
declare_wait_run $cmd -c bad_path -s script.clistate lock -i 0 -p 1

echo example: bad config file example
declare_wait_run $cmd -c badWalletCfg -s script.clistate lock -i 0 -p 1

echo example: no state arg example
declare_wait_run $cmd -c testWalletCfg lock -i 0 -p 1

echo example: no state file on inc example
declare_wait_run $cmd -c testWalletCfg -s bad_path inc

echo example: yes state file on lock
declare_wait_run $cmd -c testWalletCfg -s badState lock -i 0 -p 1

echo example: bad state file on inc example
declare_wait_run $cmd -c badWalletCfg -s badState inc
echo this error isn't great but you shouldn't be hand writing state files anyway

echo
echo Finished error examples
echo
echo Valid examples
echo
echo
echo

echo example: help page
declare_wait_run $cmd --help

echo example: lock initial value
rm script.clistate # just to make sure it doesn't exist at this point
declare_wait_run $cmd -c testWalletCfg -s script.clistate lock -i 0 -p 1

echo example: query state
declare_wait_run $cmd -c testWalletCfg -s script.clistate query

echo example: increment datum
declare_wait_run $cmd -c testWalletCfg -s script.clistate inc

echo example: query state
declare_wait_run $cmd -c testWalletCfg -s script.clistate query

echo example: end
declare_wait_run $cmd -c testWalletCfg -s script.clistate end
