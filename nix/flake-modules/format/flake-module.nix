{ self, ... }:
{
  perSystem = { config, self', inputs', system, ... }:
    let
      # TODO: We should be able to get rid of this once
      # https://github.com/numtide/treefmt/issues/165 is implemented upstream.
      # Uses https://github.com/numtide/treefmt to provide auto-formating for multiple
      # languages. Configure the behaviour in treefmt.toml at project root.
      pkgs = inputs'.nixpkgs.legacyPackages;
      ps-tools = inputs'.ps-tools.legacyPackages;
      dependencies = with pkgs; [
        haskellPackages.cabal-fmt
        haskellPackages.fourmolu
        treefmt
        nixpkgs-fmt
        ps-tools.purs-tidy
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
        format =
          pkgs.runCommandLocal "format-check" { buildInputs = dependencies; }
            ''
              set -e
              # treefmt uses a cache at $HOME. But we can use --no-cache
              # to make treefmt not use a cache. We still seem to need
              # to export a writable $HOME though.
              # TODO: https://github.com/numtide/treefmt/pull/174 fixes this issue
              # but we need to wait until a release is made and that release gets
              # into the nixpkgs we use.
              export HOME="$TMP"
              # `treefmt --fail-on-change` is broken for purs-tidy; So we must rely
              # on git to detect changes. An unintended advantage of this approach
              # is that when the check fails, it will print a helpful diff at the end.
              cp -r ${self} $HOME/project
              chmod -R a+w $HOME/project
              cd $HOME/project
              git init
              git config user.email "nix@localhost"
              git config user.name Nix
              git add .
              git commit -m init
              treefmt --no-cache
              git status
              git --no-pager diff --exit-code
              touch $out
            '';
      };
    };
  flake = { };
}
