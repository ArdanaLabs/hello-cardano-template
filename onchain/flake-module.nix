{ self, ... }:
{
  perSystem = { config, self', inputs', system, ... }:
    let
      # A flake-module in nix/flake-modules/haskell.nix defines haskell-nix
      # packages once, so we can reuse it here, it's more performant.
      pkgs =
        with self.inputs.plutarch-plutus;
        import inputs.nixpkgs {
          inherit system;
          overlays = [
            inputs.haskell-nix.overlay
            (import "${inputs.iohk-nix}/overlays/crypto")
          ];
        };
      # dusd-lib contains helper functions for dealing with haskell.nix. From it,
      # we inherit fixHaskellDotNix and some common attributes to give to
      # cabalProject'
      inherit (config) dusd-lib;
      inherit (dusd-lib.haskell)
        commonPlutusModules
        commonPlutusShell
        fixHaskellDotNix
        ;

      compiler-nix-name = "ghc923";

      plutarch-hackage =
        with self.inputs;
        plutarch-plutus.inputs.haskell-nix-extra-hackage.mkHackagesFor
          system
          compiler-nix-name
          [
            "${plutarch-plutus}"
            "${plutarch-plutus}/plutarch-extra"
          ];

      myhackage =
        self.inputs.plutarch-plutus.applyPlutarchDep pkgs {
          inherit compiler-nix-name;
          extra-hackages = plutarch-hackage.extra-hackages;
          extra-hackage-tarballs = plutarch-hackage.extra-hackage-tarballs;
          modules = plutarch-hackage.modules;
        };

      project = pkgs.haskell-nix.cabalProject' {
        src = ./.;

        cabalProjectFileName = "cabal.project";
        inherit compiler-nix-name;

        inherit (myhackage)
          extra-hackages
          extra-hackage-tarballs
          modules
          cabalProjectLocal;

        shell = commonPlutusShell // {
          # additional = myhackage;
          #ps: with ps; [
          #  # apropos
          #  # apropos-tx
          #  plutarch
          #  plutarch-extra
          #  # sydtest
          #  # sydtest-hedgehog
          #];
        };
      };

      haskellNixFlake =
        fixHaskellDotNix (project.flake { }) [ ./dUSD-onchain.cabal ];
    in
    {
      apps = {
        "onchain:test" =
          dusd-lib.mkApp
            (
              pkgs.writeShellApplication
                {
                  name = "run-onchain-tests";
                  text = ''
                    nix build -L ${self}#checks.\"${system}\".\"dUSD-onchain:test:tests\"
                    cat result/test-stdout
                  '';
                }
            );
      };
      packages =
        haskellNixFlake.packages
        // {
          "onchain:scripts" =
            pkgs.runCommand "onchain-scripts"
              { buildInputs = [ haskellNixFlake.packages."dUSD-onchain:exe:scripts" ]; }
              ''mkdir -p $out && scripts $out'';
          "onchain:hello-world-cbor-purs" =
            pkgs.runCommand "hello-world-cbor-purs" { } ''
              mkdir -p $out/src
              ${haskellNixFlake.packages."dUSD-onchain:exe:hello-world"}/bin/hello-world $out/src
            '';
        };
      checks = haskellNixFlake.checks // { };
      devShells.onchain = haskellNixFlake.devShell // { };
    };
  flake = { };
}
