{ self, ... }:
{
  perSystem = { config, self', inputs', system, ... }:
    let
      pkgs = inputs'.nixpkgs.legacyPackages;
      purs-nix = config.ps.purs-nix;
      npmlock2nix = pkgs.callPackages self.inputs.npmlock2nix { };
      inherit (purs-nix) ps-pkgs;
      inherit (config.ps) ctl-pkgs;
      inherit (config) cat-lib offchain-lib dream2nix;

      hello-world-browser = {
        ps =
          purs-nix.purs
            {
              dependencies =
                with ps-pkgs;
                [
                  halogen
                  halogen-store
                  safe-coerce
                  transformers
                  ctl-pkgs.cardano-transaction-lib
                  self'.packages."offchain:hello-world-api"
                ];
              dir = ./.;
            };
        package =
          let
            nodeModules = pkgs.symlinkJoin {
              name = "hello-world-browser-node-modules";
              paths = [
                config.ctl.nodeModules
                (npmlock2nix.node_modules { src = ./.; })
              ];
            };
          in
          pkgs.runCommand "build-hello-world-browser" { }
            # see buildPursProject: https://github.com/Plutonomicon/cardano-transaction-lib/blob/c906ead97563fef3b554320bb321afc31956a17e/nix/default.nix#L74
            # see bundlePursProject: https://github.com/Plutonomicon/cardano-transaction-lib/blob/c906ead97563fef3b554320bb321afc31956a17e/nix/default.nix#L149
            ''
              export BROWSER_RUNTIME=1
              cp -r ${hello-world-browser.ps.modules.Main.output { }} output
              cp ${./index.js} index.js
              cp ${./index.html} index.html
              cp ${./package.json} package.json
              cp ${./package-lock.json} package-lock.json
              cp -r ${nodeModules}/* .
              export NODE_PATH="node_modules"
              export PATH="bin:$PATH"
              mkdir -p $out/dist
              cp ${./index.html} $out/index.html
              postcss ${./main.css} --config=${./.postcssrc.json} > $out/dist/main.css
              webpack --mode=production -c ${../webpack.config.js} -o $out/dist --entry ./index.js
            '';
        packageWithCtlRuntimeConfig = config@{ ogmiosConfig, datumCacheConfig, ctlServerConfig }:
          pkgs.runCommand "package-with-ctl-runtime-config" { } ''
            mkdir -p $out/dist
            echo '${builtins.toJSON config}' > $out/dist/ctl-runtime-config.json
            cp -r ${hello-world-browser.package}/* $out/
            ls -lisa $out/dist
          '';
      };

      hello-world-browser-test = {
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
                  ordered-collections
                  parallel
                  self'.packages."offchain:hello-world-api"
                ];
              dir = ./.;
              srcs = [ "test/src" ];
            };
      };

      hello-world-browser-test-with-local =
        let
          testModule =
            hello-world-browser-test.ps.modules."HelloWorld.Test.Main".output
              { };
          scriptName = "hello-world-browser-test-with-local";
        in
        pkgs.writeShellApplication
          {
            name = scriptName;
            runtimeInputs = [
              pkgs.nodejs
              pkgs.chromium
              pkgs.postgresql
              self.inputs.cardano-transaction-lib.inputs.plutip.packages.${pkgs.system}."plutip:exe:plutip-server"
              self.inputs.cardano-transaction-lib.packages.${pkgs.system}."ctl-server:exe:ctl-server"
              self.inputs.mlabs-ogmios.defaultPackage.${pkgs.system}
              self.inputs.ogmios-datum-cache.defaultPackage.${pkgs.system}
              self'.packages."offchain:hello-world-browser"
            ];
            text = ''
              export MODE=local
              export NODE_PATH=${config.ctl.nodeModules}/node_modules
              export CHROME_EXE="${pkgs.chromium}/bin/chromium"
              export HELLO_WORLD_BROWSER_INDEX=${self'.packages."offchain:hello-world-browser"}
              export JQUERY_MIN_SRC="${self.inputs.jquery}/dist/jquery.min.js"

              node \
                --preserve-symlinks \
                --input-type=module \
                -e 'import { main } from "${testModule}/HelloWorld.Test.Main/index.js"; main()' \
                -- "${scriptName}" "''$@"
            '';
          };

      hello-world-browser-test-with-testnet =
        let
          testModule =
            hello-world-browser-test.ps.modules."HelloWorld.Test.Main".output
              { };
          scriptName = "hello-world-browser-test-with-testnet";
        in
        pkgs.writeShellApplication
          {
            name = scriptName;
            runtimeInputs =
              [ self'.packages."offchain:hello-world-browser" ]
              ++ (with pkgs; [ nodejs chromium unzip curl ]);
            text = ''
              export MODE=testnet
              export NODE_PATH=${config.ctl.nodeModules}/node_modules
              export CHROME_EXE="${pkgs.chromium}/bin/chromium"
              export HELLO_WORLD_BROWSER_INDEX=${self'.packages."offchain:hello-world-browser"}

              export NAMI_EXTENSION=${./test/Nami.crx}

              export NAMI_TEST_WALLET_1=${./test/NamiWallets/nami-test-wallet-1.tar.gz}
              export NAMI_TEST_WALLET_2=${./test/NamiWallets/nami-test-wallet-2.tar.gz}
              export NAMI_TEST_WALLET_3=${./test/NamiWallets/nami-test-wallet-3.tar.gz}

              node \
                --preserve-symlinks \
                --input-type=module \
                -e 'import { main } from "${testModule}/HelloWorld.Test.Main/index.js"; main()' \
                -- "${scriptName}" "''$@"
            '';
          };
    in
    {
      apps = {
        "offchain:hello-world-browser:serve" =
          cat-lib.makeServeApp self'.packages."offchain:hello-world-browser";
        "offchain:hello-world-browser:serve-live" =
          cat-lib.makeServeLive self'.packages."offchain:hello-world-browser";
        "offchain:hello-world-browser:test:local" =
          cat-lib.mkApp hello-world-browser-test-with-local;
        "offchain:hello-world-browser:test:testnet" =
          cat-lib.mkApp hello-world-browser-test-with-testnet;
      };
      checks = {
        "offchain:hello-world-browser:lighthouse" =
          pkgs.callPackage ../nixos/tests/hello-world-browser-lighthouse.nix {
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
        "offchain:hello-world-browser" =
          offchain-lib.makeProjectShell { project = hello-world-browser; };
        "offchain:hello-world-browser:test" =
          offchain-lib.makeProjectShell { project = hello-world-browser-test; };
      };
      packages = {
        "offchain:hello-world-browser" = hello-world-browser.package;
        "offchain:hello-world-browser-ctl-rt-config" = hello-world-browser.packageWithCtlRuntimeConfig { ogmiosConfig = { }; datumCacheConfig = { }; ctlServerConfig = { }; };
      };
    };
  flake = { };
}
