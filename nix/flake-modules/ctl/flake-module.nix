{ self, lib, ... }: {
  perSystem = { inputs', ... }:
  let
    pkgs = inputs'.nixpkgs.legacyPackages;
    npmlock2nix = pkgs.callPackages self.inputs.npmlock2nix { };
  in
  {
    options = with lib; {
      ctl = {
        nodeModules = mkOption {
          type = types.package;
          default =
            npmlock2nix.node_modules
              { src = self.inputs.cardano-transaction-lib; };
        };
      };
    };
  };
}
