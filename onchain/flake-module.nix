{ self, ... }:
{
  perSystem = { config, self', inputs', system, ... }:
    let
      # A flake-module in nix/flake-modules/haskell.nix defines haskell-nix
      # packages once, so we can reuse it here, it's more performant.
      pkgs = config.haskell-nix.pkgs;
      # dusd-lib contains helper functions for dealing with haskell.nix. From it,
      # we inherit fixHaskellDotNix and some common attributes to give to
      # cabalProject'
      dusd-lib = config.dusd-lib;
      inherit (dusd-lib.haskell) commonPlutusModules commonPlutusShell fixHaskellDotNix;


      plutarch = inputs'.plutarch;
      inputs.haskell-nix-extra-hackage.url = "github:mlabs-haskell/haskell-nix-extra-hackage";
      inputs.haskell-nix-extra-hackage.inputs.haskell-nix.follows = "haskell-nix";
      inputs.haskell-nix-extra-hackage.inputs.nixpkgs.follows = "nixpkgs";

      myhackage = system: compiler-nix-name: plutarch.inputs.haskell-nix-extra-hackage.mkHackageFor system compiler-nix-name (
        [
          "${inputs.flat}"
          "${inputs.protolude}"
          "${inputs.cardano-prelude}/cardano-prelude"
          "${inputs.cardano-crypto}"
          "${inputs.cardano-base}/binary"
          "${inputs.cardano-base}/cardano-crypto-class"
          "${inputs.plutus}/plutus-core"
          "${inputs.plutus}/plutus-ledger-api"
          "${inputs.plutus}/plutus-tx"
          "${inputs.plutus}/prettyprinter-configurable"
          "${inputs.plutus}/word-array"
          "${inputs.secp256k1-haskell}"
          "${inputs.plutus}/plutus-tx-plugin" # necessary for FFI tests
        ]
      );

      project = pkgs.haskell-nix.cabalProject' {
        src = pkgs.runCommand "fakesrc-onchain" { } ''
          cp -rT ${./.} $out
          chmod u+w $out/cabal.project
          cat $out/cabal-haskell.nix.project >> $out/cabal.project
        '';

        cabalProjectFileName = "cabal.project";
        compiler-nix-name = "ghc923";
        #sha256map = {} ; # import ./sha256map;

        modules = myhackage.modules ++ [{ }];
        shell = commonPlutusShell // {
          #additional = myhackage;
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

      prefixOutputs = dusd-lib.prefixAttrNames "onchain";
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
        // prefixOutputs {
          onchain-scripts =
            pkgs.runCommand "onchain-scripts"
              { buildInputs = [ haskellNixFlake.packages."dUSD-onchain:exe:scripts" ]; }
              ''mkdir -p $out && scripts $out'';

          hello-world-cbor-purs =
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
