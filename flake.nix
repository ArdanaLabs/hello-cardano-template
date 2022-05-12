{
  description = "dUSD";
  inputs = {
    haskell-nix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskell-nix/nixpkgs-unstable";
    haskell-nix.inputs.nixpkgs.follows = "haskell-nix/nixpkgs-2105";

    flake-utils.url = "github:numtide/flake-utils";
    flake-utils.inputs.nixpkgs.follows = "nixpkgs";

    cardano-node.url = "github:input-output-hk/cardano-node?rev=73f9a746362695dc2cb63ba757fbcabb81733d23";
    #   used for libsodium-vrf
    plutus.url = "github:input-output-hk/plutus";
    plutus-apps.url = "github:input-output-hk/plutus-apps?rev=e4062bca213f233cdf9822833b07aa69dff6d22a";
  };
  outputs = { self, nixpkgs, haskell-nix, flake-utils, ... }@inputs:
    let
      # Function that produces Flake outputs for the given system.
      #
      #  outputsFor :: Set Input -> System -> Set Output
      #
      # We use flake-utils.lib.eachSystem (see below) to call this.
      # cf. https://github.com/NixOS/nix/issues/3843#issuecomment-661720562
      outputsFor = system:
        let
          # TODO: We probably should use a non-haskell.nix nixpkgs for certain
          # derivations, to speed up things. Those derivations do not rely on
          # haskell.nix anyway. Consider this in the context of passing `pkgs` to
          # the psuedo flakes flakes.
          pkgs =
            import nixpkgs {
              inherit system;
              overlays = [ haskell-nix.overlay ];
              inherit (haskell-nix) config;
            };
          pseudoFlakes = {
            onchain = import ./onchain { inherit inputs system pkgs; };
            offchain = import ./offchain { inherit inputs system pkgs; };
            docs = import ./docs { inherit inputs system pkgs; };
            format = import ./nix/format.nix { inherit inputs system pkgs; };
            everything-else = import ./nix/everything-else.nix { inherit inputs system pkgs; };
            flake-local = import ./nix/flake-local.nix { inherit inputs system pkgs; };
          };

        in
        {
          inherit pseudoFlakes;

          packages =
            pseudoFlakes.onchain.packages
            // pseudoFlakes.offchain.packages
            // pseudoFlakes.docs.packages
            // pseudoFlakes.everything-else.packages;
          defaultPackage = self.packages.${system}.everything-else;

          checks =
            pseudoFlakes.onchain.checks
            // pseudoFlakes.offchain.checks
            // pseudoFlakes.format.checks;

          # We are forced to use two devshells.
          # Under ideal circumstances, we could put all the onchain and offchain
          # code in the same project, sharing the same cabal.project, but this is
          # not possible because:
          #
          # On-chain code requires recent versions of plutarch, which uses a
          # more recent version of `plutus` than is in `plutus-apps`.
          #
          # So, in order to remove this hack and use one cabal project instead, we need:
          #
          # Plutarch to be more or less stable so that it can use the version
          # of `plutus` that is in `plutus-apps` at the time, instead of a recent
          # one.
          #
          # There was also the idea of using a plutus-tx (so not Plutarch)
          # dummy-implementation of an on-chain validator until these two
          # conditions are met. We opted not to do this because it would require
          # us to bet that the condition above would be met before we want to launch.
          devShells = {
            onchain = pseudoFlakes.onchain.devShell;
            offchain = pseudoFlakes.offchain.devShell;
          };

          apps =
            pseudoFlakes.offchain.apps
            // pseudoFlakes.docs.apps
            // pseudoFlakes.format.apps;
        };
    in
    flake-utils.lib.eachSystem [ "x86_64-linux" ] outputsFor // {
      # Name of our project; used in script prefixes (eg: ghcid).
      projectName = "dusd";
    };
}
