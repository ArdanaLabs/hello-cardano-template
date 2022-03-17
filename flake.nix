{
  description = "dUSD";
  inputs = {
    haskell-nix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskell-nix/nixpkgs-unstable";
    haskell-nix.inputs.nixpkgs.follows = "haskell-nix/nixpkgs-2105";
    plutus.url = "github:input-output-hk/plutus";
    # used for libsodium-vrf
    flake-compat-ci.url = "github:hercules-ci/flake-compat-ci";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };
  outputs =
    {
      self,
      nixpkgs,
      haskell-nix,
      plutus,
      flake-compat,
      flake-compat-ci,
    }
    @ inputs:
    let
      # System types to support.
      supportedSystems = [ "x86_64-linux" "aarch64-linux" ];

      # Helper function to generate an attrset '{ x86_64-linux = f "x86_64-linux"; ... }'.
      forAllSystems = nixpkgs.lib.genAttrs supportedSystems;

      # Nixpkgs instantiated for supported system types.
      nixpkgsFor = system:
        import nixpkgs {
          inherit system;
          overlays = [ haskell-nix.overlay ];
          inherit (haskell-nix) config;
        };

      projectFor = system: let
        deferPluginErrors = true;
        pkgs = nixpkgsFor system;

        fakeSrc = pkgs.runCommand "real-source" { } ''
          cp -rT ${self} $out
          chmod u+w $out/cabal.project
          cat $out/cabal-haskell.nix.project >> $out/cabal.project
        '';
      in
        (nixpkgsFor system).haskell-nix.cabalProject' {
          src = fakeSrc.outPath;
          compiler-nix-name = "ghc8107";
          cabalProjectFileName = "cabal.project";
          modules = [
            {
              packages = {
                marlowe.flags.defer-plugin-errors = deferPluginErrors;
                plutus-use-cases.flags.defer-plugin-errors = deferPluginErrors;
                plutus-ledger.flags.defer-plugin-errors = deferPluginErrors;
                plutus-contract.flags.defer-plugin-errors = deferPluginErrors;
                cardano-crypto-praos.components.library.pkgconfig =
                  nixpkgs.lib.mkForce [ [ (import plutus { inherit system; }).pkgs.libsodium-vrf ] ];
                cardano-crypto-class.components.library.pkgconfig =
                  nixpkgs.lib.mkForce [ [ (import plutus { inherit system; }).pkgs.libsodium-vrf ] ];
              };
            }
          ];
          shell = {
            withHoogle = true;

            exactDeps = true;

            # We use the ones from Nixpkgs, since they are cached reliably.
            # Eventually we will probably want to build these with haskell.nix.
            nativeBuildInputs = [ pkgs.cabal-install pkgs.hlint pkgs.haskellPackages.fourmolu ];

            additional = ps: [
              ps.apropos
              ps.apropos-tx
              ps.plutarch
            ];
          };
          sha256map = {
            "https://github.com/mlabs-haskell/apropos"."2b2c2e01f9a3d40a3db2dfcbdb24683d584f808d" = "sha256-o9bEo9LTTfM5f/yapqLzXhxFF6XVbLTUKx1GvzJmfE0=";
            "https://github.com/mlabs-haskell/apropos-tx"."d5a90656ad77a48d2291748e1bb5ae072c85eaa4" = "sha256-SkWvW7EyI94BoFWvzyk+MsTNd3eomRlwaBovIQtI71o=";
            "https://github.com/Plutonomicon/plutarch"."aecc2050eb63ff0041576473aa3193070fe91314" = "sha256-pVvSa4fBoKXCdCu/NGduoKhr1/gGESCmj/Tr9Y5l9B4=";
            "https://github.com/input-output-hk/plutus.git"."6d8d25d1e84b2a4278da1036aab23da4161b8df8" = "o8m86TkI1dTo74YbE9CPPNrBfSDSrf//DMq+v2+woEY=";
            "https://github.com/Quid2/flat.git"."ee59880f47ab835dbd73bea0847dab7869fc20d8" = "lRFND+ZnZvAph6ZYkr9wl9VAx41pb3uSFP8Wc7idP9M=";
            "https://github.com/input-output-hk/cardano-crypto.git"."07397f0e50da97eaa0575d93bee7ac4b2b2576ec" = "oxIOVlgm07FAEmgGRF1C2me9TXqVxQulEOcJ22zpTRs=";
            "https://github.com/input-output-hk/cardano-base"."78b3928391b558fb1750228f63301ec371f13528" = "pBUTTcenaSLMovHKGsaddJ7Jh3okRTrtu5W7Rdu6RM4=";
            "https://github.com/input-output-hk/cardano-prelude"."fd773f7a58412131512b9f694ab95653ac430852" = "BtbT5UxOAADvQD4qTPNrGfnjQNgbYNO4EAJwH2ZsTQo=";
            "https://github.com/input-output-hk/Win32-network"."3825d3abf75f83f406c1f7161883c438dac7277d" = "Hesb5GXSx0IwKSIi42ofisVELcQNX6lwHcoZcbaDiqc=";
          };
        };
    in
      {
        ciNix = flake-compat-ci.lib.recurseIntoFlakeWith {
          flake = self;
          systems = [ "x86_64-linux" ];
        };

        project = forAllSystems projectFor;
        flake = forAllSystems (system: (projectFor system).flake { });

        # this could be done automatically, but would reduce readability
        packages = forAllSystems (system: self.flake.${system}.packages);
        checks = forAllSystems (system: self.flake.${system}.checks);
        check = forAllSystems (
          system:
            (nixpkgsFor system).runCommand "combined-test" {
              nativeBuildInputs = builtins.attrValues self.checks.${system};
            } "touch $out"
        );

        devShell = forAllSystems (system: self.flake.${system}.devShell);

        apps = forAllSystems (system: let
          pkgs = (forAllSystems nixpkgsFor)."${system}";
        in
          {
            feedback-loop = {
              type = "app";
              program = "${ pkgs.callPackage ./nix/apps/feedback-loop { } }/bin/feedback-loop";
            };
          });
      };
}
