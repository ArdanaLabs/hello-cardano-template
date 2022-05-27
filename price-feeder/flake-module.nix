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
      checks = {
        price-feeder-integration-test = inputs'.nixpkgs.legacyPackages.callPackage ./test.nix {
          sslCertificate = "${self.inputs.nixpkgs}/nixos/tests/common/acme/server/acme.test.cert.pem";
          sslCertificateKey = "${self.inputs.nixpkgs}/nixos/tests/common/acme/server/acme.test.key.pem";
          priceFeederExe = self'.packages."ada-price-feeder:exe:ada-price-feeder";
          binanceMockServerExe = self'.packages."binance-mock-servant-server:exe:binance-mock-servant-server";
          coinbaseMockServerExe = self'.packages."coinbase-mock-servant-server:exe:coinbase-mock-servant-server";
          huobiMockServerExe = self'.packages."huobi-mock-servant-server:exe:huobi-mock-servant-server";
          krakenMockServerExe = self'.packages."kraken-mock-servant-server:exe:kraken-mock-servant-server";
          kucoinMockServerExe = self'.packages."kucoin-mock-servant-server:exe:kucoin-mock-servant-server";
        };
        
      };
    };

  flake = {};
}
