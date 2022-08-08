{ self, ... }:
{
  perSystem = { config, self', inputs', system, ... }:
    let
      pkgs = inputs'.nixpkgs.legacyPackages;
    in
    {
      apps = {
        # Runs hlint
        lint = {
          type = "app";
          program = pkgs.writeShellApplication {
            name = "lint";
            runtimeInputs = [ pkgs.haskellPackages.hlint ];
            text = "hlint";
          };
        };
      };
      checks = {
        # Checks that there are no linter errors
        lint =
          pkgs.runCommandLocal "lint-check" { buildInputs = [ pkgs.haskellPackages.hlint ]; }
            ''
              set -euo pipefail
              hlint --report=$out ${self} 
            '';
      };
    };
  flake = { };
}
