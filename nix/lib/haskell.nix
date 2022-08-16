{ self }: {
  fixHaskellDotNix = import ./fixHaskellDotNix.nix self.inputs.nixpkgs.lib;
  mkCommonPlutusShell = compiler-nix-name: pkgs: {
    nativeBuildInputs = with pkgs; [
      cabal-install
      hlint
      (self.inputs.plutarch.hlsFor' compiler-nix-name pkgs).hsPkgs.haskell-language-server.components.exes.haskell-language-server
    ];
    exactDeps = true;
    withHoogle = true;
  };
}
