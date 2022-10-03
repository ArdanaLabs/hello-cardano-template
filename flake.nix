{

  inputs = {
    haskell-nix = {
      url = "github:input-output-hk/haskell.nix";
    };
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    cardano-node.url = "github:input-output-hk/cardano-node?rev=73f9a746362695dc2cb63ba757fbcabb81733d23";
    cardano-transaction-lib.url = "github:Plutonomicon/cardano-transaction-lib?rev=296563fa1a9a7494393bb4a74ea0464054fe9b4b";
    cardano-ogmios.url = "github:input-output-hk/cardano-ogmios";
    mlabs-ogmios.follows = "cardano-transaction-lib/ogmios";
    ogmios-datum-cache.follows = "cardano-transaction-lib/ogmios-datum-cache";
    #   used for libsodium-vrf
    plutus = {
      url = "github:input-output-hk/plutus";
    };
    plutarch = {
      url = "github:Plutonomicon/plutarch-plutus";
    };
    apropos = {
      url = "github:mlabs-haskell/apropos?rev=9dbe96f1a1108b453aaf65ade4d6280cc92cccea";
      flake = false;
    };
    digraph = {
      url = "github:mlabs-haskell/digraph?rev=d4dfec22f6a6eb646dcfa9591eaca0a9be88d260";
      flake = false;
    };
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
    lighthouse-src = {
      url = "github:GoogleChrome/lighthouse/v9.5.0";
      flake = false;
    };
    jquery = {
      url = "github:jquery/jquery/3.6.0";
      flake = false;
    };
    treefmt-flake.url = "github:srid/treefmt-flake";
    yubihsm.url = "github:ArdanaLabs/yubihsm-ed-sign?rev=43b45902d0e01ab26633d672be9143e7b3a640f3";

  };

  outputs = { self, flake-parts, treefmt-flake, ... }:
    (flake-parts.lib.evalFlakeModule
      { inherit self; }
      {
        systems = [ "x86_64-linux" ];
        imports = [
          treefmt-flake.flakeModule
          ./offchain
          ./onchain
          ./docs
          ./nix/flake-modules
          ./price-feeder
        ];
      }
    ).config.flake;
}
