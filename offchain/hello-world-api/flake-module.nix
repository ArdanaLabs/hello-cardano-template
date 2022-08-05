{ self, ... }:
{
  perSystem = { config, self', inputs', system, ... }:
    let
      pkgs = inputs'.nixpkgs.legacyPackages;
      all-ps-pkgs = config.ps.pkgs;
      purs-nix = config.ps.purs-nix;
      inherit (config) dusd-lib offchain-lib;

      hello-world-api = {
        dependencies =
          with all-ps-pkgs;
          [
            aeson
            aff
            bigints
            cardano-transaction-lib
            node-child-process
            node-fs-aff
            node-process
            ordered-collections
            spec
            self'.packages."offchain:hello-world-cbor"
          ];
        ps =
          purs-nix.purs
            {
              inherit (hello-world-api) dependencies;
              srcs = [ ./. ];
            };
        package =
          (purs-nix.build
            {
              name = "hello-world-api";
              src.path = ./.;
              info = {
                inherit (hello-world-api) dependencies;
                version = "0.0.1";
              };
            }
          )
          // { passthru = { inherit (hello-world-api) ps; }; };
      };

      hello-world-api-tests =
        let
          testModule = hello-world-api.ps.modules."Test.Main".output { };
          scriptName = "hello-world-api-tests";
        in
        pkgs.writeShellApplication
          {
            name = scriptName;
            runtimeInputs = [ pkgs.nodejs ];
            text = ''
              export TEST_RESOURCES=${./fixtures}
              export NODE_PATH=${config.ctl.nodeModules}/node_modules
              node \
                --preserve-symlinks \
                --input-type=module \
                -e 'import { main } from "${testModule}/Test.Main/index.js"; main()' \
                -- "${scriptName}" "''$@"
            '';
          };
    in
    {
      apps =
        offchain-lib.prefixOutputs
          {
            "hello-world-api:test" =
              dusd-lib.mkApp hello-world-api-tests;
          };
      packages =
        offchain-lib.prefixOutputs
          {
            hello-world-api = hello-world-api.package;
          };
      devShells =
        offchain-lib.prefixOutputs
          {
            hello-world-api =
              offchain-lib.makeProjectShell hello-world-api {};
          };
      checks.run-hello-world-api-tests =
        let test = hello-world-api-tests; in
        pkgs.runCommand test.name { NO_RUNTIME = "TRUE"; }
          "${test}/bin/${test.meta.mainProgram} | tee $out";
    };
  flake = { };
}
