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

      hello-world-api-tests =
        let testExe = hello-world-api.ps.test.run { }; in
        pkgs.writeShellApplication
          {
            name = "hello-world-api-tests";
            runtimeInputs = [
              pkgs.nodejs
              self.inputs.cardano-transaction-lib.packages.${pkgs.system}."ctl-server:exe:ctl-server"
              self.inputs.cardano-transaction-lib.inputs.plutip.packages.${pkgs.system}."plutip:exe:plutip-server"
              pkgs.postgresql
              self.inputs.mlabs-ogmios.defaultPackage.${pkgs.system}
              self.inputs.ogmios-datum-cache.defaultPackage.${pkgs.system}
            ];
            text = ''
              export NODE_PATH=${config.ctl.nodeModules}/node_modules
              ${testExe}
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
              ${hello-world-api.ps.command { }}/bin/purs-nix docs
            '';
      };
    };
  flake = { };
}
