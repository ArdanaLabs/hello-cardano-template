{ self, ... }:
{
  perSystem = { inputs', ... }: {
    checks = {
      ctl-runtime-modules-test =
        inputs'.nixpkgs.legacyPackages.callPackage
          ./tests/ctl-runtime-modules.nix
          {
            inherit (self.inputs) cardano-node cardano-ogmios;
            inherit (self.nixosModules) ctl-server ogmios-datum-cache;
          };
      hello-world-server-config-test =
        inputs'.nixpkgs.legacyPackages.callPackage ./tests/hello-world.nix {
          inherit (self.nixosConfigurations) hello-world-server;
          inherit (self.inputs) nixpkgs;
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
    nixosModules.hello-world = { pkgs, lib, ... }: {
      imports = [ ./modules/hello-world.nix ];
      services.hello-world.package = self.packages.${pkgs.system}."offchain:hello-world-browser";
    };
    nixosConfigurations = {
      hello-world-server = import ./configurations/hello-world-server.nix {
        helloWorldServerFlake = self;
        ctlServerFlake = self;
        ogmiosDatumCacheFlake = self;
        cardanoNodeFlake = self.inputs.cardano-node;
        cardanoOgmiosFlake = self.inputs.cardano-ogmios;
      };
    };
  };
}
