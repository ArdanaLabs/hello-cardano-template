{ self, ... }:
{
  perSystem = { config, self', inputs', system, ... }:
    let
      pkgs = inputs'.nixpkgs.legacyPackages;
      dependencies = [ pkgs.haskellPackages.hlint ];
    in
    {
      apps = {
        # Runs hlint
        lint = {
          type = "app";
          program = pkgs.writeShellApplication {
            name = "lint";
            runtimeInputs = dependencies;
            text = "hlint ${self}";
          };
        };
      };
      checks = {
        # Checks that there are no linter errors
        lint =
          pkgs.runCommand "lint-check" { buildInputs = dependencies; }
            ''
              set -euo pipefail
              cd ${self}  # need to do this, otherwise hlint wont pickup the correct yaml
              hlint --report=$out .
            '';
      };
    };
  flake = { };
}
