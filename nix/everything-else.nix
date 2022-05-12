# A pseudo-flake that provides a package that builds everything else not in
# 'packages' set; ie., devShells and checks.
#
# Useful as an alternative to 'nix flake check' (which doesn't work due to IFD)

{ inputs, system, pkgs }:

{
  packages = {
    # A combination of all derivations (aside from packages) we care
    # to build in CI: checks, devShells. We need this because IFD in
    # Haskell disallows use of `nix flake check`, but we can use `nix
    # build .#everything-else` to, effectively, run those checks.
    everything-else = pkgs.runCommand "everything-else"
      {
        drvs =
          builtins.attrValues inputs.self.checks.${system}
          # This allows us to cache nix-shell (nix develop)
          # https://nixos.wiki/wiki/Caching_nix_shell_build_inputs
          ++ (pkgs.lib.attrsets.mapAttrsToList
            (_: shell: shell.inputDerivation)
            inputs.self.devShells.${system});
      } ''echo $drvs > $out'';
  };
}
