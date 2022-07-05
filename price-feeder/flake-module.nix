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

      packages = haskellNixFlake.packages // {
        ada-price-feeder-test = pkgs.runCommand "ada-price-feeder-test" {
            buildInputs = [ pkgs.makeWrapper ];
          }
          ''
              mkdir -p $out/bin
              makeWrapper ${self'.packages."ada-price-feeder:exe:ada-price-feeder-test"}/bin/ada-price-feeder-test $out/bin/ada-price-feeder-test \
                --set PATH ${pkgs.lib.makeBinPath [ self'.packages."ada-price-feeder:exe:ada-price-feeder" ]}
          '';
      };

      apps = haskellNixFlake.apps;
    };

  flake = {};
}
