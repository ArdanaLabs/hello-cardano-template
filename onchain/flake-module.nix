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
      haskellNixFlake = fixHaskellDotNix (project.flake { }) [ ./dUSD-onchain.cabal ];

      project = pkgs.haskell-nix.cabalProject' {
        src = pkgs.runCommand "fakesrc-onchain" { } ''
          cp -rT ${./.} $out
          chmod u+w $out/cabal.project
          cat $out/cabal-haskell.nix.project >> $out/cabal.project
        '';
        compiler-nix-name = "ghc8107";
        cabalProjectFileName = "cabal.project";
        modules = commonPlutusModules ++ [{ }];
        shell = commonPlutusShell // {
          additional = ps: [
            ps.apropos
            ps.apropos-tx
            ps.plutarch
            ps.plutarch-extra
            ps.sydtest
            ps.sydtest-hedgehog
          ];
        };
        sha256map = import ./sha256map;
      };
    in
    {
      apps = {
        "onchain:test" =
          dusd-lib.mkRunCmdInShellApp
            {
              scriptName = "run-onchain-test";
              devshellName = "onchain";
              command = "cd onchain && cabal test";
            };
      };
      packages = haskellNixFlake.packages // {
        onchain-scripts = pkgs.stdenv.mkDerivation {
          name = "onchain-scripts";
          src = self; # FIXME: Why should src be project root here?
          buildInputs = [ haskellNixFlake.packages."dUSD-onchain:exe:scripts" ];
          doCheck = false;
          installPhase = ''
            scripts "$out"
          '';
          configurePhase = ''
            mkdir $out
          '';
        };
        hello-world-cbor-purs = pkgs.runCommand "hello-world-cbor-purs" { } ''
          mkdir -p $out/src
          ${haskellNixFlake.packages."dUSD-onchain:exe:hello-world"}/bin/hello-world $out/src
        '';
      };
      checks = haskellNixFlake.checks // { };
      devShells.onchain = haskellNixFlake.devShell // { };
    };
  flake = { };
}
