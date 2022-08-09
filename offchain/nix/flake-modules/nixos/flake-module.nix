{ self, ... }:
{
  perSystem = { inputs', ... }: {
    checks = {
      ctl-runtime-modules-test =
        inputs'.nixpkgs.legacyPackages.callPackage
          ./tests/ctl-runtime-modules.nix
          {
            inherit (self.inputs) cardano-node cardano-ogmios mlabs-ogmios;
            inherit (self.nixosModules) ctl-server ogmios-datum-cache;
          };
    };
  };
  flake = {
    nixosModules.ogmios-datum-cache = { pkgs, lib, ... }: {
      imports = [ ./modules/ogmios-datum-cache.nix ];
      services.ogmios-datum-cache.package =
        lib.mkDefault self.inputs.ogmios-datum-cache.defaultPackage.${pkgs.system};
    };
    nixosModules.ctl-server = { pkgs, lib, ... }: {
      imports = [ ./modules/ctl-server.nix ];
      services.ctl-server.package =
        lib.mkDefault self.inputs.cardano-transaction-lib.packages.${pkgs.system}."ctl-server:exe:ctl-server";
    };
  };
}
