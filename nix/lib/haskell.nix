{ self, pkgs }:
let
  lib = self.inputs.nixpkgs.lib;
in
{
  fixHaskellDotNix = import ./fixHaskellDotNix.nix self.inputs.nixpkgs.lib;
  commonPlutusShell = {
    nativeBuildInputs = with pkgs; [
      cabal-install
      hlint
    ];
    withHoogle = true;
    tools = {
      haskell-language-server = { };
    };
    exactDeps = true;
  };
  commonPlutusModules =
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
