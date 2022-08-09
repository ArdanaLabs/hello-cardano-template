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
            text = "hlint ${self}";
          };
        };
      };
      checks = {
        # Checks that there are no linter errors
        lint =
          pkgs.runCommand "lint-check" { buildInputs = [ pkgs.haskellPackages.hlint ]; }
            ''
              set -euo pipefail
              cd ${self}  # need to do this, otherwise hlint wont pickup the correct yaml
              hlint --report=$out .
            '';
      };
    };
  flake = { };
}
