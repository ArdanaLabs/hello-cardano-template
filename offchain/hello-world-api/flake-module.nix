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
            aff-retry
            self'.packages."offchain:hello-world-cbor"
          ];
        ps =
          purs-nix.purs
            {
              inherit (hello-world-api) dependencies;
              srcs = [ ./. ];
            };
        package =
          purs-nix.build
            {
              name = "hello-world-api";
              src.path = ./.;
              info = {
                inherit (hello-world-api) dependencies;
                version = "0.0.1";
              };
            };
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
      apps."offchain:hello-world-api:test" = dusd-lib.mkApp hello-world-api-tests;
      checks.run-hello-world-api-tests =
        let test = hello-world-api-tests; in
        pkgs.runCommand test.name { NO_RUNTIME = "TRUE"; }
          "${test}/bin/${test.meta.mainProgram} | tee $out";
      devShells."offchain:hello-world-api" =
        offchain-lib.makeProjectShell hello-world-api { };
      packages = {
        "offchain:hello-world-api" = hello-world-api.package;
        "offchain:hello-world-api:docs" =
          pkgs.runCommand "hello-world-api-docs" { }
            ''
              mkdir $out && cd $out
              # it may make sense to eventually add cli and browser to the srcs, but we need to not define Main twice
              ${hello-world-api.ps.command { srcs = [ ./src ];} }/bin/purs-nix docs
            '';
      };
    };
  flake = { };
}
