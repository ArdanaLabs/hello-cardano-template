{
  # nixpkgs without any overlays
  realPkgs
, # reference to flake outputs
  self
,
}: rec {
  # common utilities to work with haskell.nix
  haskell = import ./haskell.nix { inherit self; };
  # function to make a flake app
  mkApp = program: { type = "app"; inherit program; };
  # creates a flake application that when ran will serve
  # the passed path over HTTP
  makeServeApp = pathToServe:
    mkApp (
      realPkgs.writeShellApplication
        {
          name = "serve";
          runtimeInputs = [ realPkgs.simple-http-server ];
          text = "simple-http-server --ip 127.0.0.1 -p 8080 --nocache -i -- ${pathToServe}";
        }
    );
  makeServeLive = browserPackage:
    mkApp (
      realPkgs.writeShellApplication {
        name = "serve-live";
        runtimeInputs = with realPkgs; [
          entr
          findutils # for find
          procps # for pkill
          nodePackages.live-server
        ];
        text =
          let
            resultDir = "$PWD/tmp-result";
            buildBrowser =
              ''nix build .#"${browserPackage}" --out-link "${resultDir}"'';
          in
          ''
            # build once to ensure that the server has something to serve
            ${buildBrowser}
            # kill live-serve and cleanup result dir on exit
            trap 'pkill -f live-server && rm -r "${resultDir}"' EXIT
            # runs this in a subshell for the trap to kill
            (live-server "${resultDir}" &)
            # disable this shellcheck because it complains about
            # "variable won't expand in single quotes" which is what we want here.
            # shellcheck disable=SC2016
            find "$PWD/offchain" -regex ".*\(\.purs\|\.html\|\.css\)" \
              | entr -ps 'echo building; ${buildBrowser}; echo "refresh the page"'
          '';
      }
    );
}
