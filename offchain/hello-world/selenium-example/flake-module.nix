{ self, ... }:
{
  perSystem = system: { config, self', inputs', ... }:
    let
      description = "Selenium test for dUSD Hello World UI";
      # dusd-lib contains helper functions for dealing with haskell.nix. From it,
      # we inherit fixHaskellDotNix
      dusd-lib = import "${self}/nix/lib/haskell.nix" { inherit system self pkgs; };
      inherit (dusd-lib) fixHaskellDotNix;
      # realNixpkgs is required to get chromium and selenium from
      # cache.nixos.org rather than the bloated Haskell.nix Nixpkgs.
      realNixpkgs = inputs'.nixpkgs.legacyPackages;
      # A flake-module in nix/flake-modules/haskell.nix defines haskell-nix
      # packages once, so we can reuse it here, it's more performant.
      pkgs = config.haskell-nix.pkgs;
      haskellNixFlake =
        fixHaskellDotNix (project.flake { })
          [ ./selenium-example.cabal ];
      project = pkgs.haskell-nix.cabalProject {
        modules = [{
          packages = {
            selenium-example.components.exes.sydtest-webdriver = {
              pkgconfig = [ [ realNixpkgs.makeWrapper ] ];
              postInstall = with realNixpkgs; ''
                wrapProgram $out/bin/sydtest-webdriver \
                  --set FONTCONFIG_FILE ${makeFontsConf { fontDirectories = [ twitter-color-emoji roboto ]; }} \
                  --prefix PATH : "${realNixpkgs.lib.makeBinPath [
                    chromedriver
                    chromium
                    selenium-server-standalone
                  ]}"
              '';
            };
          };
        }];
        name = "selenium-example";
        src = ./.;
        compiler-nix-name = "ghc8107";
        sha256map = import ./sha256map;
        # This is used by `nix develop .` to open a shell for use with
        # `cabal`, `hlint` and `haskell-language-server`
        shell = {
          tools = {
            cabal = { };
            hlint = { };
            haskell-language-server = { };
          };
          buildInputs = with realNixpkgs; [
            chromedriver
            chromium
            selenium-server-standalone
            nixpkgs-fmt
          ];
        };
      };
    in
    {
      packages = haskellNixFlake.packages;
      devShells.selenium-example = haskellNixFlake.devShell;
      checks = haskellNixFlake.checks // { };
    };
  flake = { };
}
