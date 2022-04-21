{
  description = "dUSD";
  inputs = {
    haskell-nix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskell-nix/nixpkgs-unstable";
    haskell-nix.inputs.nixpkgs.follows = "haskell-nix/nixpkgs-2105";
    #   used for libsodium-vrf
    plutus.url = "github:input-output-hk/plutus";
    lint-utils = {
      type = "git";
      url = "https://gitlab.homotopic.tech/nix/lint-utils.git";
      ref = "spec-type";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
  outputs =
    {
      self,
      nixpkgs,
      haskell-nix,
      plutus,
      lint-utils
    }
    @ inputs:
    let
      # System types to support.
      supportedSystems = [ "x86_64-linux" ]; #"aarch64-linux" ];

      # Helper function to generate an attrset '{ x86_64-linux = f "x86_64-linux"; ... }'.
      forAllSystems = nixpkgs.lib.genAttrs supportedSystems;

      # Nixpkgs instantiated for supported system types.
      nixpkgsFor = system:
        import nixpkgs {
          inherit system;
          overlays = [ haskell-nix.overlay ];
          inherit (haskell-nix) config;
        };

      # Derivation for a Haskell Plutus project that lives in the sub-directory of this mono repo.
      plutusProjectIn = 
        { system
        , subdir       # The sub-directory name
        , extraShell   # Extra 'shell' attributes used by haskell.nix
        , sha256map    # Extra sha256 hashes used by haskell.nix
        }: 
        let
          deferPluginErrors = true;
          pkgs = nixpkgsFor system;
          fakeSrc = pkgs.runCommand "real-source-${subdir}" { } ''
            cp -rT ${self}/${subdir} $out
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
              tools = {
                haskell-language-server = { };
              };
              exactDeps = true;
              # We use the ones from Nixpkgs, since they are cached reliably.
              # Eventually we will probably want to build these with haskell.nix.
              nativeBuildInputs = [ pkgs.cabal-install pkgs.hlint pkgs.haskellPackages.fourmolu ];
            } // extraShell;
            inherit sha256map;
          } ;

      lintSpec = {
        cabal-fmt = {};
        fourmolu = {
          ghcOpts = "-o-XTypeApplications -o-XImportQualifiedPost";
        };
        # Enable after https://github.com/ArdanaLabs/dUSD/issues/8
        # nixpkgs-fmt = {};
      };

      # Checks the shell script using ShellCheck
      checkedShellScript = system: name: text:
        ((nixpkgsFor system).writeShellApplication {
          inherit name text;
        }) + "/bin/${name}";

      # Take a flake app (identified as the key in the 'apps' set), and return a
      # derivation that runs it in the compile phase.
      #
      # In effect, this allows us to run an 'app' as part of the build process (eg: in CI).
      flakeApp2Derivation = system: appName:
        (nixpkgsFor system).runCommand appName { } "${self.apps.${system}.${appName}.program} | tee $out";
    in
      {
        onchain-scripts = forAllSystems (system: (nixpkgsFor system).stdenv.mkDerivation {
          name = "onchain-scripts";
          src = self;
          buildInputs = [ self.onchain.${system}.flake.packages."dUSD-onchain:exe:scripts" ];
          doCheck = false;
          installPhase = ''
            scripts "$out"
            '';
          configurePhase = ''
            mkdir $out
            '';
        });

        onchain = forAllSystems (system: rec {
          project = plutusProjectIn {
            inherit system;
            subdir = "onchain";
            extraShell = {
              additional = ps: [
                ps.apropos
                ps.apropos-tx
                ps.plutarch
                ps.plutarch-extra
                ps.sydtest
                ps.sydtest-hedgehog
              ];
            };
            sha256map = {
              "https://github.com/mlabs-haskell/apropos"."455b1a3ad1eee35de4d3fb8c4a4527071474336c" = "sha256-EC6vnimXA+jBRPQLLs2dltuTx9XoSdkQfh742NnLJSQ=";
              "https://github.com/mlabs-haskell/apropos-tx"."489eeb8c30d62d5c75eafe4242a1f133695f8564" = "sha256-15nFGPhXBy+G0oocb6KQf5KVnT0fuAOoFCdzT+vyeEg=";
              "https://github.com/Plutonomicon/plutarch"."cae000584f5492daa2e13a08632ea69221a8c147" = "sha256-ZEGKBr1q852sx5aOOAqWHKZR+//YByQmGmBKglGr+qQ=";
              "https://github.com/input-output-hk/plutus.git"."983e6af2154c4bdf86ed645062bcb62f304d0a4f" = "sha256-Ga+hIhrgq2kR5Vnso/Edo2wgQpFn167eWGl35oM093U=";
              "https://github.com/Quid2/flat.git"."ee59880f47ab835dbd73bea0847dab7869fc20d8" = "lRFND+ZnZvAph6ZYkr9wl9VAx41pb3uSFP8Wc7idP9M=";
              "https://github.com/input-output-hk/cardano-crypto.git"."07397f0e50da97eaa0575d93bee7ac4b2b2576ec" = "oxIOVlgm07FAEmgGRF1C2me9TXqVxQulEOcJ22zpTRs=";
              "https://github.com/input-output-hk/cardano-base"."78b3928391b558fb1750228f63301ec371f13528" = "pBUTTcenaSLMovHKGsaddJ7Jh3okRTrtu5W7Rdu6RM4=";
              "https://github.com/input-output-hk/cardano-prelude"."fd773f7a58412131512b9f694ab95653ac430852" = "BtbT5UxOAADvQD4qTPNrGfnjQNgbYNO4EAJwH2ZsTQo=";
              "https://github.com/input-output-hk/Win32-network"."3825d3abf75f83f406c1f7161883c438dac7277d" = "Hesb5GXSx0IwKSIi42ofisVELcQNX6lwHcoZcbaDiqc=";
              "https://github.com/Srid/sydtest"."9c6c7678f7aabe22e075aab810a6a2e304591d24" = "sha256-P6ZwwfFeN8fpi3fziz9yERTn7BfxdE/j/OofUu+4GdA=";
              "https://github.com/Srid/autodocodec"."42b42a7407f33c6c74fa4e8c84906aebfed28daf" = "sha256-X1TNZlmO2qDFk3OL4Z1v/gzvd3ouoACAiMweutsYek4=";
              "https://github.com/Srid/validity"."f7982549b95d0ab727950dc876ca06b1862135ba" = "sha256-dpMIu08qXMzy8Kilk/2VWpuwIsfqFtpg/3mkwt5pdjA=";
            };
          };
          flake = project.flake { };
        });

        offchain = forAllSystems (system: rec {
          project = plutusProjectIn {
            inherit system;
            subdir = "offchain";
            extraShell = {
              additional = ps: [
                ps.plutarch
              ];
              DUSD_SCRIPTS = self.onchain-scripts.${system};
            };
            sha256map = {
              "https://github.com/Plutonomicon/plutarch"."4052b285eb890799332c0cbe19cb08c1070f267a" = "sha256-8Tbrd9nPUkZarQiUTWYnNwbuz8wRhu+ipRK4XyedjTs=";
              "https://github.com/input-output-hk/plutus.git"."983e6af2154c4bdf86ed645062bcb62f304d0a4f" = "sha256-Ga+hIhrgq2kR5Vnso/Edo2wgQpFn167eWGl35oM093U=";
              "https://github.com/Quid2/flat.git"."ee59880f47ab835dbd73bea0847dab7869fc20d8" = "lRFND+ZnZvAph6ZYkr9wl9VAx41pb3uSFP8Wc7idP9M=";
              "https://github.com/input-output-hk/cardano-crypto.git"."07397f0e50da97eaa0575d93bee7ac4b2b2576ec" = "oxIOVlgm07FAEmgGRF1C2me9TXqVxQulEOcJ22zpTRs=";
              "https://github.com/input-output-hk/cardano-base"."78b3928391b558fb1750228f63301ec371f13528" = "pBUTTcenaSLMovHKGsaddJ7Jh3okRTrtu5W7Rdu6RM4=";
              "https://github.com/input-output-hk/cardano-prelude"."fd773f7a58412131512b9f694ab95653ac430852" = "BtbT5UxOAADvQD4qTPNrGfnjQNgbYNO4EAJwH2ZsTQo=";
              "https://github.com/input-output-hk/Win32-network"."3825d3abf75f83f406c1f7161883c438dac7277d" = "Hesb5GXSx0IwKSIi42ofisVELcQNX6lwHcoZcbaDiqc=";
            };
          };
          flake = project.flake { };
        });

        # this could be done automatically, but would reduce readability
        packages = forAllSystems (system:
          let pkgs = (forAllSystems nixpkgsFor)."${system}";
          in self.onchain.${system}.flake.packages
          // self.offchain.${system}.flake.packages
          // {
            build-docs = pkgs.stdenv.mkDerivation {
              name = "build-docs";
              src = self;
              buildInputs = with pkgs; [ (texlive.combine { inherit ( texlive ) scheme-basic latexmk todonotes metafont; }) ];
              doCheck = false;
              buildPhase = ''
                HOME=$TMP latexmk -output-directory="tmp" -pdf ./docs/*.tex
                mkdir $out -p
                cp tmp/*.pdf $out
              '';
              installPhase = ''
                ls -lah
              '';
            };
        });

        checks = forAllSystems (system:
             self.onchain.${system}.flake.checks
          // { offchain-test = flakeApp2Derivation system "offchain-test";
             }
          // (lint-utils.mkChecks.${system} lintSpec ./.)
        );
        # We need this attribute because `nix flake check` won't work for Haskell
        # projects: https://nixos.wiki/wiki/Import_From_Derivation#IFD_and_Haskell
        #
        # Instead, run: `nix build .#check.x86_64-linux` (replace with your system)
        check = forAllSystems (
          system:
            (nixpkgsFor system).runCommand "combined-test" {
              checksss = builtins.attrValues self.checks.${system}
              # This allows us to cache nix-shell (nix develop)
              # https://nixos.wiki/wiki/Caching_nix_shell_build_inputs
              ++ [
                self.devShells.${system}.onchain.inputDerivation
                self.devShells.${system}.offchain.inputDerivation
              ];
            } ''
              echo $checksss
              touch $out
              ''
        );

        devShells = forAllSystems (system: {
          onchain = self.onchain.${system}.flake.devShell;
          offchain = self.offchain.${system}.flake.devShell;
        });
        defaultPackage = forAllSystems (system:
             self.packages.${system}."dUSD-onchain:test:tests"
          // self.packages.${system}."dUSD-offchain:exe:tests"
        );
        apps = forAllSystems (system: let
          pkgs = (forAllSystems nixpkgsFor)."${system}";
        in
          {
            feedback-loop = {
              type = "app";
              program = "${ pkgs.callPackage ./nix/apps/feedback-loop { } }/bin/feedback-loop";
            };
            format =  lint-utils.mkApp.${system} lintSpec;
            offchain-test = {
              type = "app";
              program = checkedShellScript system "dUSD-offchain-test"
                '' export DUSD_SCRIPTS=${self.onchain-scripts.${system}}
                   cd ${self}
                   ${self.packages.${system}."dUSD-offchain:exe:tests"}/bin/tests;
                '';
            };
          });
      };
}
