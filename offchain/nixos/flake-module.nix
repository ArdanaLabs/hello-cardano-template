{ self, ... }:
{
  perSystem = { inputs', ... }: {
    checks = {
      ctl-runtime-test =
        inputs'.nixpkgs.legacyPackages.callPackage
          ./tests/ctl-runtime.nix
          {
            inherit (self.nixosModules) ctl-runtime;
          };
      hello-world-test =
        inputs'.nixpkgs.legacyPackages.callPackage ./tests/hello-world.nix {
          inherit (self.nixosModules) hello-world;
          inherit (self.inputs) nixpkgs;
        };
    };
  };
  flake = {
    nixosModules = {
      ogmios-datum-cache = { pkgs, lib, ... }: {
        imports = [
          self.inputs.cardano-node.nixosModules.cardano-node
          self.inputs.cardano-ogmios.nixosModules.ogmios
          ./modules/ogmios-datum-cache.nix
        ];
        services.ogmios-datum-cache.package =
          lib.mkDefault self.inputs.ogmios-datum-cache.defaultPackage.${pkgs.system};
      };
      ctl-server = { pkgs, lib, ... }: {
        imports = [ ./modules/ctl-server.nix ];
        services.ctl-server.package = lib.mkDefault self.inputs.cardano-transaction-lib.packages.${pkgs.system}."ctl-server:exe:ctl-server";
      };
      ctl-runtime = { pkgs, config, ... }: {
        imports = [
          self.nixosModules.ogmios-datum-cache
          self.nixosModules.ctl-server
          ./modules/ctl-runtime.nix
        ];
      };
      hello-world = { pkgs, lib, ... }: {
        imports = [
          self.nixosModules.ctl-runtime
          ./modules/hello-world.nix
        ];
        services.hello-world.package = self.packages.${pkgs.system}."offchain:hello-world-browser";
      };
    };
  };
}
