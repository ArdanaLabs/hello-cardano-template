{ self, ... }:
{
  perSystem = { config, self', inputs', system, ... }:
    let
      pkgs = inputs'.nixpkgs.legacyPackages;
      purs-nix = config.ps.purs-nix;
      npmlock2nix = pkgs.callPackages self.inputs.npmlock2nix { };
      inherit (purs-nix) ps-pkgs;
      inherit (config.ps) ctl-pkgs;
      inherit (config) dusd-lib offchain-lib dream2nix;

      hello-world-browser-key-wallet = {
        ps =
          purs-nix.purs
            {
              dependencies =
                with ps-pkgs;
                [
                  aff
                  bigints
                  halogen
                  halogen-store
                  safe-coerce
                  transformers
                  ctl-pkgs.cardano-transaction-lib
                  self'.packages."offchain:hello-world-api"
                ];
              dir = ./.;
              srcs = [ "src/KeyWallet" "src/HelloWorld" ];
            };
        package =
          let
            nodeModules = pkgs.symlinkJoin {
              name = "hello-world-browser-key-wallet-node-modules";
              paths = [
                config.ctl.nodeModules
                (npmlock2nix.node_modules { src = ./.; })
              ];
            };
          in
          pkgs.runCommand "build-hello-world-browser-key-wallet"
            {
              buildInputs = [
                pkgs.postgresql
                self.inputs.cardano-transaction-lib.inputs.plutip.packages.${pkgs.system}."plutip:exe:plutip-server"
                self.inputs.cardano-transaction-lib.packages.${pkgs.system}."ctl-server:exe:ctl-server"
                self.inputs.mlabs-ogmios.defaultPackage.${pkgs.system}
                self.inputs.ogmios-datum-cache.defaultPackage.${pkgs.system}
              ];
            }
            # see buildPursProject: https://github.com/Plutonomicon/cardano-transaction-lib/blob/c906ead97563fef3b554320bb321afc31956a17e/nix/default.nix#L74
            # see bundlePursProject: https://github.com/Plutonomicon/cardano-transaction-lib/blob/c906ead97563fef3b554320bb321afc31956a17e/nix/default.nix#L149
            ''
              mkdir $out && cd $out
              export BROWSER_RUNTIME=1
              cp -r ${hello-world-browser-key-wallet.ps.modules."KeyWallet.Main".output { }} output
              cp ${./KeyWallet.js} index.js
              cp ${./index.html} index.html
              cp ${./package.json} package.json
              cp ${./package-lock.json} package-lock.json
              cp ${./.postcssrc.json} .postcssrc.json
              cp ${../webpack.config.js} webpack.config.js
              cp -r ${nodeModules}/* .
              export NODE_PATH="node_modules"
              export PATH="bin:$PATH"
              mkdir dist
              postcss ${./main.css} --config=.postcssrc.json > dist/main.css
              webpack --mode=production -c webpack.config.js -o ./dist --entry ./index.js
            '';
      };

      hello-world-browser-nami-wallet = {
        ps =
          purs-nix.purs
            {
              dependencies =
                with ps-pkgs;
                [
                  aff
                  bigints
                  halogen
                  halogen-store
                  safe-coerce
                  transformers
                  ctl-pkgs.cardano-transaction-lib
                  self'.packages."offchain:hello-world-api"
                ];
              dir = ./.;
              srcs = [ "src/NamiWallet" "src/HelloWorld" ];
            };
        package =
          let
            nodeModules = pkgs.symlinkJoin {
              name = "hello-world-browser-nami-wallet-node-modules";
              paths = [
                config.ctl.nodeModules
                (npmlock2nix.node_modules { src = ./.; })
              ];
            };
          in
          pkgs.runCommand "build-hello-world-browser-nami-wallet" { }
            # see buildPursProject: https://github.com/Plutonomicon/cardano-transaction-lib/blob/c906ead97563fef3b554320bb321afc31956a17e/nix/default.nix#L74
            # see bundlePursProject: https://github.com/Plutonomicon/cardano-transaction-lib/blob/c906ead97563fef3b554320bb321afc31956a17e/nix/default.nix#L149
            ''
              mkdir $out && cd $out
              export BROWSER_RUNTIME=1
              cp -r ${hello-world-browser-nami-wallet.ps.modules."NamiWallet.Main".output { }} output
              cp ${./NamiWallet.js} index.js
              cp ${./index.html} index.html
              cp ${./package.json} package.json
              cp ${./package-lock.json} package-lock.json
              cp ${./.postcssrc.json} .postcssrc.json
              cp ${../webpack.config.js} webpack.config.js
              cp -r ${nodeModules}/* .
              export NODE_PATH="node_modules"
              export PATH="bin:$PATH"
              mkdir dist
              postcss ${./main.css} --config=.postcssrc.json > dist/main.css
              webpack --mode=production -c webpack.config.js -o ./dist --entry ./index.js
            '';
      };

      hello-world-browser-e2e = {
        ps =
          purs-nix.purs
            {
              dependencies =
                with ps-pkgs;
                [
                  aff
                  ctl-pkgs.cardano-transaction-lib
                  express
                  mote
                  node-process
                  test-unit
                  ctl-pkgs.toppokki
                  node-child-process
                  parallel
                ];
              dir = ./.;
              srcs = [ "test/e2e/src" ];
            };
      };

      hello-world-browser-e2e-with-key-wallet =
        let
          testModule =
            hello-world-browser-e2e.ps.modules."HelloWorld.Test.E2E.Main".output
              { };
          scriptName = "hello-world-browser-e2e-with-key-wallet";
        in
        pkgs.writeShellApplication
          {
            name = scriptName;
            runtimeInputs =
              [ self'.packages."offchain:hello-world-browser:key-wallet" ]
              ++ (with pkgs; [ nodejs chromium ]);
            text = ''
              export LC_ALL=C.utf-8
              # this fixes a postgresql issue for me (Brian)
              # I think this is related https://github.com/NixOS/nixpkgs/issues/60414
              export TEST_WALLET="KeyWallet"
              export NODE_PATH=${config.ctl.nodeModules}/node_modules
              export CHROME_EXE="${pkgs.chromium}/bin/chromium"
              export HELLO_WORLD_BROWSER_INDEX=${self'.packages."offchain:hello-world-browser:key-wallet"}

              node \
                --preserve-symlinks \
                --input-type=module \
                -e 'import { main } from "${testModule}/HelloWorld.Test.E2E.Main/index.js"; main()' \
                -- "${scriptName}" "''$@"
            '';
          };

      hello-world-browser-e2e-with-nami-wallet =
        let
          testModule =
            hello-world-browser-e2e.ps.modules."HelloWorld.Test.E2E.Main".output
              { };
          scriptName = "hello-world-browser-e2e-with-nami-wallet";
        in
        pkgs.writeShellApplication
          {
            name = scriptName;
            runtimeInputs =
              [ self'.packages."offchain:hello-world-browser:nami-wallet" ]
              ++ (with pkgs; [ nodejs chromium unzip ]);
            text = ''
              export TEST_WALLET="NamiWallet"
              export NODE_PATH=${config.ctl.nodeModules}/node_modules
              export CHROME_EXE="${pkgs.chromium}/bin/chromium"
              export HELLO_WORLD_BROWSER_INDEX=${self'.packages."offchain:hello-world-browser:nami-wallet"}

              export NAMI_EXTENSION="${self.inputs.cardano-transaction-lib}/test-data/chrome-extensions/nami_3.2.5_1.crx"

              export NAMI_TEST_WALLET_1=${./test/e2e/NamiWallets/nami-test-wallet-1.tar.gz}
              export NAMI_TEST_WALLET_2=${./test/e2e/NamiWallets/nami-test-wallet-2.tar.gz}
              export NAMI_TEST_WALLET_3=${./test/e2e/NamiWallets/nami-test-wallet-3.tar.gz}

              node \
                --preserve-symlinks \
                --input-type=module \
                -e 'import { main } from "${testModule}/HelloWorld.Test.E2E.Main/index.js"; main()' \
                -- "${scriptName}" "''$@"
            '';
          };
    in
    {
      apps = {
        "offchain:hello-world-browser:serve" =
          dusd-lib.makeServeApp self'.packages."offchain:hello-world-browser";
        "offchain:hello-world-browser:serve-live" =
          dusd-lib.mkApp (
            pkgs.writeShellApplication {
              name = "hello-world-browser-serve-loop";
              runtimeInputs = with pkgs; [
                entr
                findutils # for find
                procps # for pkill
                nodePackages.live-server
              ];
              text =
                let
                  resultDir = "$PWD/tmp-result";
                  buildBrowser =
                    ''nix build .#"offchain:hello-world-browser" --out-link "${resultDir}"'';
                in
                ''
                  # build once to ensure that the server has something to serve
                  ${buildBrowser}
                  # kill live-serve and cleanup result dir on exit
                  trap 'pkill -f live-server && rm -r "${resultDir}"' EXIT
                  # runs this in a subshell for the trap to kill
                  (live-server "${resultDir}" &)
                  # disable this shellcheck because it complains about
                  # "variable won't expand in single quotes" which is what we want here.
                  # shellcheck disable=SC2016
                  find "$PWD/offchain" -regex ".*\(\.purs\|\.html\|\.css\)" \
                    | entr -ps 'echo building; ${buildBrowser}; echo "refresh the page"'
                '';
            }
          );
        "offchain:hello-world-browser:e2e:key-wallet" =
          dusd-lib.mkApp hello-world-browser-e2e-with-key-wallet;
        "offchain:hello-world-browser:e2e:nami-wallet" =
          dusd-lib.mkApp hello-world-browser-e2e-with-nami-wallet;
      };
      checks = {
        "offchain:hello-world-browser:e2e:key-wallet" =
          let test = hello-world-browser-e2e-with-key-wallet; in
          pkgs.runCommand test.name { }
            "${test}/bin/${test.meta.mainProgram} | tee $out";
        "offchain:hello-world-browser:e2e:nami-wallet" =
          let test = hello-world-browser-e2e-with-nami-wallet; in
          pkgs.runCommand test.name { NO_RUNTIME = "TRUE"; }
            "${test}/bin/${test.meta.mainProgram} | tee $out";
        "offchain:hello-world-browser:lighthouse" =
          pkgs.callPackage ../../nixos/tests/hello-world-browser-lighthouse.nix {
            lighthouse =
              (dream2nix.lib.makeOutputs { source = self.inputs.lighthouse-src; }).packages.lighthouse;
            hello-world-browser = self'.packages."offchain:hello-world-browser";
            # TODO these values need to be increased once the improvements were done
            categories = {
              performance = 0.1;
              accessibility = 0.1;
              seo = 0.1;
              best-practices = 0.1;
            };
          };
      };
      devShells = {
        "offchain:hello-world-browser:key-wallet" =
          offchain-lib.makeProjectShell hello-world-browser-key-wallet { };
        "offchain:hello-world-browser:nami-wallet" =
          offchain-lib.makeProjectShell hello-world-browser-nami-wallet { };
        "offchain:hello-world-browser:e2e" =
          offchain-lib.makeProjectShell hello-world-browser-e2e { };
      };
      packages = {
        "offchain:hello-world-browser:key-wallet" = hello-world-browser-key-wallet.package;
        "offchain:hello-world-browser:nami-wallet" = hello-world-browser-nami-wallet.package;
      };
    };
  flake = { };
}
