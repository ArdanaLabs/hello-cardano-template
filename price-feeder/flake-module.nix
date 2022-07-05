{ self, ... }:
{
  perSystem = system: { config, self', inputs', ... }:
    let
      description = "The price fetcher clients and price feeder for the price-module";
      # A flake-module in nix/flake-modules/haskell.nix defines haskell-nix
      # packages once, so we can reuse it here, it's more performant.
      pkgs = config.haskell-nix.pkgs;
      price-feeder = pkgs.haskell-nix.cabalProject {
        name = "price-feeder";
        src = ./.;
        compiler-nix-name = "ghc8107";
        shell = {
          withHoogle = false;
          tools = {
            cabal = { };
            ghcid = { };
            haskell-language-server = { };
          };
          exactDeps = true;
        };
      };
      haskellNixFlake = price-feeder.flake { };
    in
    {
      devShells.price-feeder = haskellNixFlake.devShell;

      packages = haskellNixFlake.packages;

      apps = haskellNixFlake.apps;
    };

  flake = {};
}
