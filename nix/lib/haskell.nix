{ self }:
let
  lib = self.inputs.nixpkgs.lib;
in
{
  fixHaskellDotNix = import ./fixHaskellDotNix.nix self.inputs.nixpkgs.lib;
  commonPlutusShell = compiler-nix-name: pkgs: {
    nativeBuildInputs = with pkgs; [
      cabal-install
      hlint
      (self.inputs.plutarch.hlsFor' compiler-nix-name pkgs).hsPkgs.haskell-language-server.components.exes.haskell-language-server
    ];
    exactDeps = true;
    withHoogle = true;
  };
  commonPlutusModules = pkgs:
    let
      deferPluginErrors = true;
      commonPkgconfigDeps = [
        [
          (import self.inputs.plutus { inherit (pkgs) system; }).pkgs.libsodium-vrf
        ]
      ];
    in
    [{
      packages = {
        marlowe.flags.defer-plugin-errors = deferPluginErrors;
        plutus-use-cases.flags.defer-plugin-errors = deferPluginErrors;
        plutus-ledger.flags.defer-plugin-errors = deferPluginErrors;
        plutus-contract.flags.defer-plugin-errors = deferPluginErrors;
        cardano-crypto-praos.components.library.pkgconfig =
          lib.mkForce commonPkgconfigDeps;
        cardano-crypto-class.components.library.pkgconfig =
          lib.mkForce commonPkgconfigDeps;
      };
    }];
}
