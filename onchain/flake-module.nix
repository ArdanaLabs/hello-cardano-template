{ self, ... }:
{
  perSystem = { config, self', inputs', system, ... }:
    let
      inherit (self.inputs) plutarch;
      # We use plutarch's nixpkgs / haskell.nix etc. to make sure that we don't
      # bother with mixing and matching nixpkgs / haskell.nix versions.
      pkgs =
        import plutarch.inputs.nixpkgs {
          inherit system;
          overlays = [
            plutarch.inputs.haskell-nix.overlay
            (import "${plutarch.inputs.iohk-nix}/overlays/crypto")
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
        plutarch.inputs.haskell-nix-extra-hackage.mkHackagesFor
          system
          compiler-nix-name
          [
            "${plutarch}"
            "${plutarch}/plutarch-extra"
          ];
      myhackage =
        plutarch.applyPlutarchDep pkgs {
          inherit compiler-nix-name;
          inherit (plutarch-hackage)
            extra-hackages
            extra-hackage-tarballs
            modules
            ;
        };

      project = pkgs.haskell-nix.cabalProject' {
        src = ./.;

        inherit compiler-nix-name;

        inherit (myhackage)
          extra-hackages
          extra-hackage-tarballs
          modules
          cabalProjectLocal
          ;

        shell = commonPlutusShell // {
          additional = ps: with ps; [
            # apropos
            # apropos-tx
            plutarch
            plutarch-extra
            # sydtest
            # sydtest-hedgehog
          ];
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
