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
    lint-utils = {
      type = "git";
      url = "https://gitlab.homotopic.tech/nix/lint-utils.git";
      ref = "overengineered";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      haskell-nix,
      cardano-node,
      plutus,
      plutus-apps,
      lint-utils
    }
    @ inputs:
    let
      # Function that produces Flake outputs for the given system.
      #
      #  outputsFor :: Set Input -> System -> Set Output
      #
      # We use flake-utils.lib.eachSystem (see below) to call this.
      # cf. https://github.com/NixOS/nix/issues/3843#issuecomment-661720562
      outputsFor = system:
    # NOTE: The body of this function is de-indented twice to keep diff clean.
    # We should use nixpkgs-fmt later in a single commit.
    let
      # TODO: We probably should use a non-haskell.nix nixpkgs for certain
      # derivations, to speed up things. Those derivations do not rely on
      # haskell.nix anyway.
      pkgs = 
        import nixpkgs {
          inherit system;
          overlays = [ haskell-nix.overlay ];
          inherit (haskell-nix) config;
        };
      inherit (pkgs.callPackage ./nix/plutus.nix { inherit system pkgs self plutus; }) 
        plutusProjectIn;
      onchain = pkgs.callPackage ./onchain { inherit system pkgs self plutus; };
      offchain = pkgs.callPackage ./offchain { 
        inherit system pkgs self plutus cardano-node plutus-apps; 
        onchain-scripts = onchain.onchain-scripts; 
      };

      # Name of our project; used in script prefixes.
      projectName = "dusd";

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
        (pkgs.writeShellApplication {
          inherit name text;
        }) + "/bin/${name}";

      # Take a flake app (identified as the key in the 'apps' set), and return a
      # derivation that runs it in the compile phase.
      #
      # In effect, this allows us to run an 'app' as part of the build process (eg: in CI).
      flakeApp2Derivation = system: appName:
        pkgs.runCommand appName { } "${self.apps.${system}.${appName}.program} | tee $out";
    in
      {

        # this could be done automatically, but would reduce readability
        packages =
          onchain.flake.packages
          // offchain.flake.packages
          // {
            build-docs = pkgs.callPackage ./docs { inherit pkgs; };
        };

        checks =
             onchain.flake.checks
          // {
               offchain = {
                 test = flakeApp2Derivation system "offchain-test";
                 hello-world.unit = self.packages.${system}."hello-world:test:hello-world-unit";
                 hello-world.e2e = self.packages.${system}."hello-world:exe:hello-world-e2e";
               };
             }
          // (lint-utils.mkChecks.${system} lintSpec ./.);

        # We need this attribute because `nix flake check` won't work for Haskell
        # projects: https://nixos.wiki/wiki/Import_From_Derivation#IFD_and_Haskell
        #
        # Instead, run: `nix build .#check.x86_64-linux` (replace with your system)
        check =
            pkgs.runCommand "combined-test" {
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
              '';

        # Shell tools common to both onchain and offchain
        commonTools = {
          feedback-loop = pkgs.callPackage ./nix/apps/feedback-loop { inherit projectName; };
        };

        ghcid = subProject: name: args: 
          pkgs.callPackage ./nix/apps/ghcid {
            inherit projectName args;
            name = "${subProject}-ghcid-${name}";
            cabalProjectRoot = "${self.flakeRoot.${system}.envVar}/${subProject}";
          };

        onchainTools = {
          onchain-ghcid-lib = self.ghcid.${system} "onchain" "lib" "-c 'cabal repl'";
          onchain-ghcid-test = self.ghcid.${system} "onchain" "test" "-c 'cabal repl test:tests'";
          onchain-ghcid-test-run = self.ghcid.${system} "onchain" "test-run" "-c 'cabal repl test:tests' -T :main";
        };

        offchainTools = {
          offchain-ghcid-lib = self.ghcid.${system} "offchain" "lib" "-c 'cabal repl'";
          offchain-ghcid-test = self.ghcid.${system} "offchain" "test" "-c 'cabal repl exe:tests'";
          offchain-ghcid-test-run = self.ghcid.${system} "offchain" "test-run" "-c 'cabal repl exe:tests' -T :main";
        };

        # In Nix, there is no builtin way to access the project root, where
        # flake.nix lives. To workaround this, we inject it as env var in the
        # `shellHook`.
        flakeRoot = {
          shellHook = ''
            export FLAKE_ROOT=$(pwd)
          '';
          envVar = "$FLAKE_ROOT";
        };

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
          onchain = onchain.flake.devShell.overrideAttrs (oa: {
            shellHook = oa.shellHook + self.flakeRoot.${system}.shellHook;
            buildInputs = pkgs.lib.attrsets.attrValues (
              self.commonTools.${system} //
              self.onchainTools.${system}
            );
          });
          offchain = offchain.flake.devShell.overrideAttrs (oa: {
            buildInputs = pkgs.lib.attrsets.attrValues (
              self.commonTools.${system} //
              self.offchainTools.${system}
            );
            shellHook = oa.shellHook + ''
              ${self.flakeRoot.${system}.shellHook}
              # running local cluster + PAB
              export SHELLEY_TEST_DATA="${plutus-apps}/plutus-pab/local-cluster/cluster-data/cardano-node-shelley/"
            '';
          });
        };
        defaultPackage =
             self.packages.${system}."dUSD-onchain:test:tests"
          // self.packages.${system}."dUSD-offchain:exe:tests";
        apps = let
          # Take a set of derivations, and return a set of apps.
          #
          # The name of the app is determined from the set keys. The derivation
          # is expected to contain a binary named `${projectName}-${key}`.
          appsFromDerivationSet = drvs: 
            pkgs.lib.attrsets.mapAttrs (name: value: {
              type = "app";
              program = "${value}/bin/${projectName}-${name}";
            }) drvs;
          # Apps that are also available in the shell. An app named `.#foo` can
          # be run inside the shell as `dusd-foo`.
          shellApps =  
            appsFromDerivationSet (
                 self.commonTools.${system} 
              # NOTE: ghcid can only run inside nix-shell, so we cannot run these as flake apps.
              # // self.onchainTools.${system} 
              # // self.offchainTools.${system}
            );
        in
          shellApps // 
          {
            format =  lint-utils.mkApp.${system} lintSpec;  # TODO: Refactor this by moving it to appsFromDerivationSet
            offchain-test = {
              type = "app";
              program = checkedShellScript system "dUSD-offchain-test"
                '' export DUSD_SCRIPTS=${onchain.onchain-scripts}
                   cd ${self}
                   ${self.packages.${system}."dUSD-offchain:exe:tests"}/bin/tests;
                '';
            };
          };
      };
  in flake-utils.lib.eachSystem [ "x86_64-linux" ] outputsFor;
}
