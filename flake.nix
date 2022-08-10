{
  inputs = {
    haskell-nix = {
      url = "github:input-output-hk/haskell.nix";
    };
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    cardano-node.url = "github:input-output-hk/cardano-node?rev=73f9a746362695dc2cb63ba757fbcabb81733d23";
    cardano-transaction-lib.url = "github:Plutonomicon/cardano-transaction-lib?rev=6cdbad6e3250101ac909c582be743c9ab2566d99";
    cardano-ogmios.url = "github:input-output-hk/cardano-ogmios";
    mlabs-ogmios.follows = "cardano-transaction-lib/ogmios";
    ogmios-datum-cache.follows = "cardano-transaction-lib/ogmios-datum-cache";
    #   used for libsodium-vrf
    plutus.url = "github:input-output-hk/plutus";
    lint-utils = {
      type = "git";
      url = "https://gitlab.homotopic.tech/nix/lint-utils.git";
      ref = "overengineered";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    flake-parts = {
      url = "github:hercules-ci/flake-parts";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    dream2nix = {
      url = "github:davhau/dream2nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    npmlock2nix = {
      flake = false;
      url = "github:nix-community/npmlock2nix";
    };
    ps-tools.follows = "purs-nix/ps-tools";
    # ps-0.14 is the branch for Purescript 0.14
    # which we use because ctl uses it
    purs-nix.url = "github:ursi/purs-nix/ps-0.14";
  };

  outputs = { self, flake-parts, ... }:
    (flake-parts.lib.evalFlakeModule
      { inherit self; }
      {
        systems = [ "x86_64-linux" ];
        imports = [
          ./offchain
          ./onchain
          ./docs
          ./nix/flake-modules
          ./price-feeder
        ];
      }
    ).config.flake;
}
