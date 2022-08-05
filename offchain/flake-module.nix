{ self, lib, ... }:
{
  perSystem = { config, self', inputs', system, ... }:
    let
      pkgs = inputs'.nixpkgs.legacyPackages;
      purs-nix = config.ps.purs-nix;
      all-ps-pkgs = config.ps.pkgs;
      inherit (config) dusd-lib offchain-lib;

      # Ideally we would just append the CTL overlay to the haskell-nix pkgs
      # we already have at `config.haskell-nix.pkgs`, but our haskell-nix
      # instances seem to be incompatible. So we just use CTLs haskell-nix here.
      ctl-pkgs = import self.inputs.nixpkgs {
        inherit system;
        overlays = with self.inputs.cardano-transaction-lib; [
          inputs.haskell-nix.overlay
          inputs.iohk-nix.overlays.crypto
          overlays.runtime
        ];
      };

      # use more recent slot to avoid long sync time
      ctlRuntimeConfig = {
        datumCache.blockFetcher.firstBlock = {
          slot = 62153233;
          id = "631c621b7372445acf82110282ba72f4b52dafa09c53864ddc2e58be24955b2a";
        };
      };

      hello-world-cbor =
        purs-nix.build
          {
            name = "hello-world-cbor";
            src.path = self'.packages."onchain:hello-world-cbor-purs";
            info.dependencies = [ ];
            info.version = "0.0.1";
          };
    in
    {
      apps = {
        ctl-runtime = ctl-pkgs.launchCtlRuntime ctlRuntimeConfig;
        "offchain:docs:serve" =
          dusd-lib.makeServeApp
            "${self'.packages."offchain:docs"}/generated-docs/html/";
      };
      packages = {
        "offchain:hello-world-cbor" = hello-world-cbor;
        "offchain:docs" =
          pkgs.runCommand "offchain-docs" { }
            ''
              mkdir $out && cd $out
              # it may make sense to eventually add cli and browser to the srcs, but we need to not define Main twice
              ${self'.packages."offchain:hello-world-api".passthru.ps.command { srcs = [ ./hello-world-api/src ];} }/bin/purs-nix docs
            '';
      };
    };
}
