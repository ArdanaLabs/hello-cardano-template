{ self, system }:
{
  fixHaskellDotNix = import ./fixHaskellDotNix.nix self.inputs.nixpkgs.lib;
  # Derivation for a Haskell Plutus project that lives in the sub-directory of this mono repo.
  plutusProjectIn =
    { pkgs            # The instance of nixpkgs to use
    , subdir          # The sub-directory name
    , extraShell      # Extra 'shell' attributes used by haskell.nix
    , pkg-def-extras  # For overriding the package set
    , sha256map       # Extra sha256 hashes used by haskell.nix
    , extraPackages ? { }
    }:
    let
      deferPluginErrors = true;
      fakeSrc = pkgs.runCommand "real-source-${subdir}" { } ''
        cp -rT ${self}/${subdir} $out
        chmod u+w $out/cabal.project
        cat $out/cabal-haskell.nix.project >> $out/cabal.project
      '';
    in
    pkgs.haskell-nix.cabalProject' {
      inherit pkg-def-extras;
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
              pkgs.lib.mkForce [ [ (import self.inputs.plutus { inherit system; }).pkgs.libsodium-vrf ] ];
            cardano-crypto-class.components.library.pkgconfig =
              pkgs.lib.mkForce [ [ (import self.inputs.plutus { inherit system; }).pkgs.libsodium-vrf ] ];
          } // extraPackages;
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
        nativeBuildInputs = [
          pkgs.cabal-install
          pkgs.hlint
        ];
#        ++
#        # Auto-formatter dependencies are useful in devshell for editors.
#        self.pseudoFlakes.${system}.format.dependencies;
      } // extraShell;
      inherit sha256map;
    };
}
