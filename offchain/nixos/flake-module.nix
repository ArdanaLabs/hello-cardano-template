{ self, ... }:
{
  perSystem = { inputs', ... }: {
    checks = {
      ctl-runtime-modules-test =
        inputs'.nixpkgs.legacyPackages.callPackage
          ./tests/ctl-runtime-modules.nix
          {
            inherit (self.nixosModules) ctl-runtime;
          };
      hello-world-server-config-test =
        inputs'.nixpkgs.legacyPackages.callPackage ./tests/hello-world.nix {
          inherit (self.nixosModules) ctl-runtime hello-world-server;
          inherit (self.inputs) nixpkgs;
        };
    };
  };
  flake = {
    nixosModules = {
      ogmios-datum-cache = { pkgs, lib, ... }: {
        imports = [ ./modules/ogmios-datum-cache.nix ];
        services.ogmios-datum-cache.package =
          lib.mkDefault self.inputs.ogmios-datum-cache.defaultPackage.${pkgs.system};
      };
      ctl-server = { pkgs, lib, ... }: {
        imports = [ ./modules/ctl-server.nix ];
        services.ctl-server.package =
          lib.mkDefault self.inputs.cardano-transaction-lib.packages.${pkgs.system}."ctl-server:exe:ctl-server";
      };
      hello-world = { pkgs, lib, ... }: {
        imports = [ ./modules/hello-world.nix ];
        services.hello-world.package = self.packages.${pkgs.system}."offchain:hello-world-browser";
      };
      ctl-runtime = { pkgs, config, ... }: {
        imports = [
          self.inputs.cardano-node.nixosModules.cardano-node
          self.inputs.cardano-ogmios.nixosModules.ogmios
          self.nixosModules.ogmios-datum-cache
          self.nixosModules.ctl-server
          ./configurations/ctl-runtime.nix
        ];
      };
      hello-world-server = { pkgs, config, ... }: {
        imports = [
          self.nixosModules.hello-world
          ./configurations/hello-world-server.nix
        ];
      };
    };
  };
}
