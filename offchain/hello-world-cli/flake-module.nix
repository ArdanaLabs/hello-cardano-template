{ self, ... }:
{
  perSystem = { config, self', inputs', system, ... }:
    let
      pkgs = inputs'.nixpkgs.legacyPackages;
      purs-nix = config.ps.purs-nix;
      all-ps-pkgs = config.ps.pkgs;
      inherit (config) dusd-lib offchain-lib;

      hello-world-cli = {
        ps =
          purs-nix.purs
            {
              dependencies =
                with all-ps-pkgs;
                [
                  prelude
                  optparse
                  node-fs-aff
                  node-fs
                  dotenv
                  stringutils
                  self'.packages."offchain:hello-world-api"
                ];
              srcs = [ ./. ];
            };
        package =
          let js = "${hello-world-cli.ps.modules.Main.output {}}/Main/index.js"; in
          pkgs.writeScriptBin "hello-world-cli"
            ''
              export NODE_PATH=${config.ctl.nodeModules}/node_modules
              ${pkgs.nodejs}/bin/node \
                --preserve-symlinks \
                --input-type=module \
                -e 'import { main } from "${js}"; main()' \
                -- "hello-world-cli" "''$@"
            '';
      };

      hello-world-cli-tests =
        let
          testExe =
            hello-world-cli.ps.modules."Test.Main".app
              { name = scriptName; };
          scriptName = "hello-world-cli-tests";
        in
        pkgs.writeShellApplication
          {
            name = scriptName;
            runtimeInputs = [
              testExe
              self'.packages."offchain:hello-world-cli"
              pkgs.coreutils
            ];
            text = ''
              export TEST_RESOURCES=${./fixtures}
              ${scriptName}
            '';
          };
    in
    {
      apps =
        offchain-lib.prefixOutputs
          {
            "hello-world-cli:test" =
              dusd-lib.mkApp hello-world-cli-tests;
          };
      packages =
        offchain-lib.prefixOutputs
          {
            hello-world-cli = hello-world-cli.package;
          };
      devShells =
        offchain-lib.prefixOutputs
          {
            hello-world-cli =
              offchain-lib.makeProjectShell hello-world-cli { };
          };
      checks.run-hello-world-cli-tests =
        let test = hello-world-cli-tests; in
        pkgs.runCommand test.name { NO_RUNTIME = "TRUE"; }
          "${test}/bin/${test.meta.mainProgram} | tee $out";
    };
  flake = { };
}
