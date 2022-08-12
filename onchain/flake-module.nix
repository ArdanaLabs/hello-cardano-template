{ self, ... }:
{
  perSystem = { config, self', inputs', system, ... }:
    let
      # A flake-module in nix/flake-modules/haskell.nix defines haskell-nix
      # packages once, so we can reuse it here, it's more performant.
      inherit (config.haskell-nix) pkgs;
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

      myhackage =
        self.inputs.haskell-nix-extra-hackage.mkHackagesFor system compiler-nix-name
          (with self.inputs; [
            # TODO: where do we get these from?
            "${plutarch-plutus.inputs.protolude}"
            "${plutarch-plutus.inputs.secp256k1-haskell}"
            "${plutarch-plutus.inputs.flat}"
            "${cardano-transaction-lib.inputs.cardano-prelude}/cardano-prelude"
            "${cardano-transaction-lib.inputs.cardano-crypto}"
            "${cardano-transaction-lib.inputs.cardano-base}/binary"
            "${cardano-transaction-lib.inputs.cardano-base}/cardano-crypto-class"
            "${plutarch-plutus}"
            "${plutarch-plutus}/plutarch-extra"
            "${plutus}/plutus-core"
            "${plutus}/plutus-ledger-api"
            "${plutus}/plutus-tx"
            "${plutus}/prettyprinter-configurable"
            "${plutus}/word-array"
            "${plutus}/plutus-tx-plugin" # necessary for FFI tests
          ])
      ;

      project = pkgs.haskell-nix.cabalProject' {
        src = pkgs.runCommand "fakesrc-onchain" { } ''
          cp -rT ${./.} $out
          chmod u+w $out/cabal.project
          cat $out/cabal-haskell.nix.project >> $out/cabal.project
        '';

        cabalProjectFileName = "cabal.project";
        inherit compiler-nix-name;

        modules = myhackage.modules;
        extra-hackages = myhackage.extra-hackages;
        extra-hackage-tarballs = myhackage.extra-hackage-tarballs;
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
                  runtimeInputs = [ pkgs.nix ];
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
