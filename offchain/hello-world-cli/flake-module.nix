{ self, ... }:
{
  perSystem = { config, self', inputs', system, ... }:
    let
      pkgs = inputs'.nixpkgs.legacyPackages;
      purs-nix = config.ps.purs-nix;
      inherit (purs-nix) ps-pkgs;
      inherit (config) cat-lib offchain-lib;

      hello-world-cli = {
        ps =
          purs-nix.purs
            {
              dependencies =
                with ps-pkgs;
                [
                  prelude
                  optparse
                  node-fs-aff
                  node-fs
                  dotenv
                  stringutils
                  bigints
                  self'.packages."offchain:hello-world-api"
                ];
              dir = ./.;
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

      hello-world-cli-tests = mode:
        let testExe = hello-world-cli.ps.test.run { }; in
        pkgs.writeShellApplication
          {
            name = "hello-world-cli-tests";
            runtimeInputs = [
              pkgs.coreutils
              self'.packages."offchain:hello-world-cli"
              inputs'.yubihsm.packages.default
            ] ++ pkgs.lib.optionals (mode == "local") [
              pkgs.postgresql
              self.inputs.cardano-transaction-lib.inputs.plutip.packages.${pkgs.system}."plutip:exe:plutip-server"
              self.inputs.cardano-transaction-lib.packages.${pkgs.system}."ctl-server:exe:ctl-server"
              self.inputs.mlabs-ogmios.defaultPackage.${pkgs.system}
              self.inputs.ogmios-datum-cache.defaultPackage.${pkgs.system}
            ];
            text = ''
              export MODE=${mode}
              export NODE_PATH=${config.ctl.nodeModules}/node_modules
              export TEST_RESOURCES=${./fixtures}
              ${testExe}
            '';
          };
    in
    {
      apps."offchain:hello-world-cli:test:local" = cat-lib.mkApp (hello-world-cli-tests "local");
      apps."offchain:hello-world-cli:test:testnet" = cat-lib.mkApp (hello-world-cli-tests "testnet");
      checks.run-hello-world-cli-tests =
        let test = hello-world-cli-tests "local"; in
        pkgs.runCommand test.name { }
          "${test}/bin/${test.meta.mainProgram} | tee $out";
      devShells."offchain:hello-world-cli" =
        offchain-lib.makeProjectShell { project = hello-world-cli; extraBuildInputs = [ inputs'.yubihsm.packages.default ]; };
      packages."offchain:hello-world-cli" = hello-world-cli.package;
    };
  flake = { };
}
