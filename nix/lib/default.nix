{
  # nixpkgs with haskell-nix
  pkgs
, # nixpkgs without any overlays
  realPkgs
, # reference to flake outputs
  self
,
}: {
  # common utilities to work with haskell.nix
  haskell = pkgs.callPackage ./haskell.nix {
    inherit self;
  };
  # function to make a flake app that runs some shell commands from
  # the root of the repository.
  # optionally can take `devshellName` and `devshellCommand` to run
  # inside the command inside the specified devshell.
  mkScript = realPkgs.callPackage ./mkScript.nix { };
}
