{

  inputs = {
    haskell-nix = {
      url = "github:input-output-hk/haskell.nix";
    };
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    # Use divnix/blank to fix CI failing due to broken cardano-node flake
    cardano-node = {
      url = "github:input-output-hk/cardano-node?ref=1.35.4";
      inputs.cardano-node-workbench.follows = "cardano-node-workbench";
      inputs.node-measured.follows = "cardano-node-workbench";
    };
    cardano-node-workbench = {
      url = "github:input-output-hk/cardano-node/ed9932c52aaa535b71f72a5b4cc0cecb3344a5a3";
      inputs.membench.follows = "empty-flake";
    };
    empty-flake.url = "github:input-output-hk/empty-flake?rev=2040a05b67bf9a669ce17eca56beb14b4206a99a";
    cardano-transaction-lib.url = "github:Plutonomicon/cardano-transaction-lib?rev=67ae3d1a3e7128cb7f58a5b4ba365aae37133045";
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
    yubihsm.url = "github:ArdanaLabs/yubihsm-ed-sign?rev=6fc4b462fc400cc2058df81f760228c2088db8d4";

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
        ];
      }
    ).config.flake;
}
