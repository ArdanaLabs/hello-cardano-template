{ self, ... }:
{
  perSystem = system: { config, self', inputs', ... }:
    let
      description = "Selenium test for dUSD Hello World UI";
      # A flake-module in nix/flake-modules/haskell.nix defines haskell-nix
      # packages once, so we can reuse it here, it's more performant.
      pkgs = config.haskell-nix.pkgs;
      seleniumExample = pkgs.haskell-nix.cabalProject {
        name = "selenium-example";
        src = ./.;
        compiler-nix-name = "ghc8107";
        sha256map = { "https://github.com/NorfairKing/sydtest"."a446095558bc9f67bc3b8ca2e9ce230150c049ff" = "sha256-grXhl2KfpQWmGpbPGJUyELwQaHfLPICyVZNMLkctj1Q="; };
        # This is used by `nix develop .` to open a shell for use with
        # `cabal`, `hlint` and `haskell-language-server`
        shell = {
          tools = {
            cabal = {};
            hlint = {};
            haskell-language-server = {};
          };
          buildInputs = with pkgs; [
            inputs'.nixpkgs'.legacyPackages.selenium-server-standalone
            nixpkgs-fmt
          ];
        };
        # Non-Haskell shell tools go here
        # This adds `js-unknown-ghcjs-cabal` to the shell.
        # shell.crossPlatforms = p: [p.ghcjs];
        };
      haskellNixFlake = seleniumExample.flake { };

      selenium-example-test = seleniumExample.selenium-example.components.exes.sydtest-webdriver.overrideAttrs (oldAttrs: rec {
          buildInputs = oldAttrs.buildInputs or [] ++ [ pkgs.makeWrapper ];
          postInstall = oldAttrs.postInstall or "" + ''
            wrapProgram $out/bin/sydtest-webdriver \
            --set PATH ${pkgs.lib.makeBinPath [ pkgs.selenium-server-standalone ]}
          '';
        });
    in
    {
      packages = {
        # TODO @Matthew, run this selenium example test in a nixos vm and add it to checks
        inherit selenium-example-test;
      };
      devShells.selenium-example = haskellNixFlake.devShell;
      checks = {
        
      };
    };

  flake = {};
}
