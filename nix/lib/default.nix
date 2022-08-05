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
  # function  to prefix the names of an attrset with a string
  # it will prefix like `${prefix}:${attrName}`.
  prefixAttrNames = prefix: attrs:
    with realPkgs.lib;
    mapAttrs'
      (n: v: nameValuePair "${prefix}:${n}" v)
      attrs;
  # creates a flake application that when ran will serve
  # the passed path over HTTP
  makeServeApp = pathToServe:
    mkApp (
      pkgs.writeShellApplication
        {
          name = "serve";
          runtimeInputs = [ pkgs.nodePackages.http-server ];
          text = "http-server -c-1 ${pathToServe}";
        }
    );
}
