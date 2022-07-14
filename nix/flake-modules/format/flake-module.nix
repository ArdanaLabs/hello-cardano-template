{ self, ... }:
{
  perSystem = { config, self', inputs', system, ... }:
    let
      # TODO: We should be able to get rid of this once
      # https://github.com/numtide/treefmt/issues/165 is implemented upstream.
      # Uses https://github.com/numtide/treefmt to provide auto-formating for multiple
      # languages. Configure the behaviour in treefmt.toml at project root.
      pkgs = inputs'.nixpkgs.legacyPackages;
      dependencies = with pkgs; [
        haskellPackages.cabal-fmt
        haskellPackages.fourmolu
        treefmt
        nixpkgs-fmt
      ];
    in
    {
      apps = {
        # Formats the project tree.
        format = {
          type = "app";
          program = pkgs.writeShellApplication {
            name = "format";
            runtimeInputs = dependencies;
            text = "treefmt";
          };
        };
      };
      checks = {
        # Checks that the project tree is *already* formatted.
        format = pkgs.runCommandLocal "format-check"
          {
            buildInputs = dependencies;
          } ''
          set -e
          # treefmt uses a cache at $HOME. But we can use --no-cache
          # to make treefmt not use a cache. We still seem to need
          # to export a writable $HOME though.
          # TODO: https://github.com/numtide/treefmt/pull/174 fixes this issue
          # but we need to wait until a release is made and that release gets
          # into the nixpkgs we use.
          export HOME="$TMP"
          treefmt -vvv --no-cache --fail-on-change -C ${self}
          touch $out
        '';
      };
    };
  flake = { };
}
