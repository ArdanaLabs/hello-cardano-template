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
  # function to make a flake app that runs a command in a given devshell
  mkRunCmdInShellApp = realPkgs.callPackage ./mkRunCmdInShellApp.nix { };
}
