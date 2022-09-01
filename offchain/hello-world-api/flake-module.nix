{ self, ... }:
{
  perSystem = { config, self', inputs', system, ... }:
    let
      pkgs = inputs'.nixpkgs.legacyPackages;
      purs-nix = config.ps.purs-nix;
      inherit (purs-nix) ps-pkgs;
      inherit (config.ps) ctl-pkgs;
      inherit (config) dusd-lib offchain-lib;

      hello-world-api = {
        dependencies =
          with ps-pkgs;
          [
            ctl-pkgs.aeson
            aff
            bigints
            ctl-pkgs.cardano-transaction-lib
            node-fs-aff
            ordered-collections
            aff-retry
            self'.packages."offchain:hello-world-cbor"
          ];
        test-dependencies =
          with ps-pkgs;
          [
            node-process
            spec
          ];
        ps =
          purs-nix.purs
            {
              inherit (hello-world-api) dependencies test-dependencies;
              dir = ./.;
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

      hello-world-api-tests = mode:
        pkgs.writeShellApplication
          {
            name = "hello-world-api-tests";
            runtimeInputs = [
              pkgs.nodejs
            ] ++ pkgs.lib.optionals (mode == "local") [
              pkgs.postgresql
              self.inputs.cardano-transaction-lib.inputs.plutip.packages.${pkgs.system}."plutip:exe:plutip-server"
              self.inputs.cardano-transaction-lib.packages.${pkgs.system}."ctl-server:exe:ctl-server"
              self.inputs.mlabs-ogmios.defaultPackage.${pkgs.system}
              self.inputs.ogmios-datum-cache.defaultPackage.${pkgs.system}
            ];
            text = ''
              export MODE=${mode}
              export TEST_RESOURCES=${./fixtures}
              export NODE_PATH=${config.ctl.nodeModules}/node_modules
              ${hello-world-api.ps.test.run { }}
            '';
          };
    in
    {
      apps = {
        "offchain:hello-world-api:test:testnet" = dusd-lib.mkApp (hello-world-api-tests "testnet");
        "offchain:hello-world-api:test:local" = dusd-lib.mkApp (hello-world-api-tests "local");
      };
      checks.run-hello-world-api-tests =
        let test = hello-world-api-tests "local"; in
        pkgs.runCommand test.name { }
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
              ${hello-world-api.ps.command { srcs = [ ./src ];}}/bin/purs-nix docs
            '';
      };
    };
  flake = { };
}
