{ self, ... }:
{
  perSystem = { config, self', inputs', system, ... }:
    let
      pkgs = inputs'.nixpkgs.legacyPackages;
      purs-nix = config.ps.purs-nix;
      inherit (purs-nix) ps-pkgs;
      inherit (config.ps) ctl-pkgs;
      inherit (config) dusd-lib offchain-lib;

      hello-world-browser = {
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
            };
        package =
          pkgs.runCommand "build-hello-world-browser" { }
            # see buildPursProject: https://github.com/Plutonomicon/cardano-transaction-lib/blob/c906ead97563fef3b554320bb321afc31956a17e/nix/default.nix#L74
            # see bundlePursProject: https://github.com/Plutonomicon/cardano-transaction-lib/blob/c906ead97563fef3b554320bb321afc31956a17e/nix/default.nix#L149
            ''
              mkdir $out && cd $out
              export BROWSER_RUNTIME=1
              cp -r ${hello-world-browser.ps.modules.Main.output { }} output
              cp ${./index.js} index.js
              cp ${./index.html} index.html
              cp ${../webpack.config.js} webpack.config.js
              cp -r ${config.ctl.nodeModules}/* .
              export NODE_PATH="node_modules"
              export PATH="bin:$PATH"
              mkdir dist
              cp ${./main.css} dist/main.css
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

      hello-world-browser-tests =
        let
          testModule =
            hello-world-browser-e2e.ps.modules."HelloWorld.Test.E2E.Main".output
              { };
          scriptName = "hello-world-browser-tests";
        in
        pkgs.writeShellApplication
          {
            name = scriptName;
            runtimeInputs =
              [ self'.packages."offchain:hello-world-browser" ]
              ++ (with pkgs; [ nodejs chromium unzip coreutils ]);
            text = ''
              export NODE_PATH=${config.ctl.nodeModules}/node_modules
              export CHROME_EXE="${pkgs.chromium}/bin/chromium"
              export HELLO_WORLD_BROWSER_INDEX=${self'.packages."offchain:hello-world-browser"}

              export NAMI_EXTENSION="${self.inputs.cardano-transaction-lib}/test-data/chrome-extensions/nami_3.2.5_1.crx"

              export NAMI_TEST_WALLET_1=${./test/e2e/TestWallets/nami-test-wallet-1.tar.gz}
              export NAMI_TEST_WALLET_2=${./test/e2e/TestWallets/nami-test-wallet-2.tar.gz}
              export NAMI_TEST_WALLET_3=${./test/e2e/TestWallets/nami-test-wallet-3.tar.gz}

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
        "offchain:hello-world-browser:test" =
          dusd-lib.mkApp hello-world-browser-tests;
      };
      checks.run-hello-world-browser-tests =
        let test = hello-world-browser-tests; in
        pkgs.runCommand test.name { NO_RUNTIME = "TRUE"; }
          "${test}/bin/${test.meta.mainProgram} | tee $out";
      devShells = {
        "offchain:hello-world-browser" =
          offchain-lib.makeProjectShell hello-world-browser { };
        "offchain:hello-world-browser:e2e" =
          offchain-lib.makeProjectShell hello-world-browser-e2e { };
      };
      packages."offchain:hello-world-browser" = hello-world-browser.package;
    };
  flake = { };
}
