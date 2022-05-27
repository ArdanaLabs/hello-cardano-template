{ self, ... }:
{
  perSystem = system: { config, self', inputs', ... }:
  let
      # A flake-module in nix/flake-modules/haskell.nix defines haskell-nix
      # packages once, so we can reuse it here, it's more performant.
      pkgs = config.haskell-nix.pkgs;
      # dusd-lib contains helper functions for dealing with haskell.nix. From it,
      # we inherit fixHaskellDotNix and some common attributes to give to
      # cabalProject'
      dusd-lib = import "${self}/nix/lib/haskell.nix" { inherit system self pkgs; };
      inherit (dusd-lib) commonPlutusModules commonPlutusShell fixHaskellDotNix;
      haskellNixFlake = fixHaskellDotNix (project.flake {}) [ ./dUSD-onchain.cabal ];

      project = pkgs.haskell-nix.cabalProject' {
        src = ./.;
        compiler-nix-name = "ghc8107";
        cabalProjectFileName = "cabal.project";
        modules = commonPlutusModules ++ [{
        }];
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
      };
      checks = haskellNixFlake.checks // {
      };
      devShells.onchain = haskellNixFlake.devShell // { };
    };
  flake = {
  };
}
