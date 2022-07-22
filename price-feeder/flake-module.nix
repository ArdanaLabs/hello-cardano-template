{ self, ... }:
{
  perSystem = { config, self', inputs', ... }:
    let
      description = "The price fetcher clients and price fetcher for the price-module";
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
      apps = haskellNixFlake.apps;

      checks = {
        ada-price-fetcher-test =
          pkgs.runCommand "ada-price-fetcher-test"
            { buildInputs = [ pkgs.makeWrapper ]; }
            ''
              mkdir -p $out/bin
              makeWrapper ${self'.packages."ada-price-fetcher:exe:ada-price-fetcher-test"}/bin/ada-price-fetcher-test $out/bin/ada-price-fetcher-test \
                --set PATH ${pkgs.lib.makeBinPath [ self'.packages."ada-price-fetcher:exe:ada-price-fetcher" ]}
            '';
      };

      devShells.price-feeder = haskellNixFlake.devShell;

      packages = haskellNixFlake.packages;
    };
  flake = { };
}
