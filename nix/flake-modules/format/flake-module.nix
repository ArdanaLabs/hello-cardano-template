{ self, ... }:
{
  perSystem = system: { config, self', inputs', ... }:
    let
      # TODO: We should be able to get rid of this once
      # https://github.com/numtide/treefmt/issues/165 is implemented upstream.
      # Uses https://github.com/numtide/treefmt to provide auto-formating for multiple
      # languages. Configure the behaviour in treefmt.toml at project root.
      pkgs = inputs'.nixpkgs.legacyPackages;
      dependencies = [
        pkgs.haskellPackages.cabal-fmt
        pkgs.haskellPackages.fourmolu
        pkgs.treefmt
      ];
    in
    {
      apps = {
        format = {
          type = "app";
          program = pkgs.writeShellApplication
            {
              name = "format";
              runtimeInputs = dependencies;
              text = "treefmt";
            } + "/bin/format";
        };
      };
      checks = {
        # Checks that the project tree is *already* formatted.
        format = pkgs.runCommandLocal "format-check"
          {
            buildInputs = dependencies;
          } ''
          set -e
          # treefmt maintains a cache at ~/.cache; so use $TMP as home.
          # treefmt 0.4 has a --no-cache option, but our nixpkgs has only 0.3
          export HOME=$TMP
          treefmt -vvv --clear-cache --fail-on-change -C ${self}
          touch $out
        '';
      };
    };
  flake = {
  };
}
