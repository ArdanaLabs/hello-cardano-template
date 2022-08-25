{ self, ... }:
{
  perSystem = { config, self', inputs', system, pkgs, ... }:
    let
      ps-tools = inputs'.ps-tools.legacyPackages;
    in
    {
      treefmt.formatters = {
        inherit (pkgs) nixpkgs-fmt;
        inherit (pkgs.haskellPackages)
          cabal-fmt
          fourmolu;
        inherit (ps-tools) purs-tidy;
      };
    };
  flake = { };
}
