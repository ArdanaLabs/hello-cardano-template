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
}
