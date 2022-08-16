{
  # nixpkgs with haskell-nix
  pkgs
, # nixpkgs without any overlays
  realPkgs
, # reference to flake outputs
  self
,
}: rec {
  # common utilities to work with haskell.nix
  haskell = pkgs.callPackage ./haskell.nix {
    inherit self;
  };
  # function to make a flake app
  mkApp = program: { type = "app"; inherit program; };
  # creates a flake application that when ran will serve
  # the passed path over HTTP
  makeServeApp = pathToServe:
    mkApp (
      pkgs.writeShellApplication
        {
          name = "serve";
          runtimeInputs = [ pkgs.simple-http-server ];
          text = "simple-http-server --ip 127.0.0.1 -p 8080 --nocache -i -- ${pathToServe}";
        }
    );
}
