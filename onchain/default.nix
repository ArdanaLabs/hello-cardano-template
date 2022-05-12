{ inputs, system }:

let
  inherit (inputs.nixpkgs.legacyPackages.${system}.callPackage ../nix/haskell.nix { inherit inputs system; })
    pkgs plutusProjectIn ghcid;
  subdir = "onchain";
in
rec {
  project = plutusProjectIn {
    inherit subdir;
    extraShell = {
      additional = ps: [
        ps.apropos
        ps.apropos-tx
        ps.plutarch
        ps.plutarch-extra
        ps.sydtest
        ps.sydtest-hedgehog
      ];
    };
    pkg-def-extras = [ ];
    sha256map = {
      "https://github.com/mlabs-haskell/apropos"."8304b8a91d38ea26d3302aee411748176c85c4bd" = "sha256-TqCQixWcs492kcJlWffyjwixPwG4LYATqy6lwbzA6DA=";
      "https://github.com/mlabs-haskell/apropos-tx"."9734e1e8a4833fc56af350adf07c104cf043aafa" = "sha256-QpHYio6n3DciQXIJOoH65DTbclSQSrTPS0nbeGKK33c=";
      "https://github.com/mlabs-haskell/digraph"."d4dfec22f6a6eb646dcfa9591eaca0a9be88d260" = "sha256-ytQkJ18tYs13rt66s4jdbaGa5mLNEIerF8u24PvyPLA=";
      "https://github.com/Plutonomicon/plutarch"."ae2059f11f24d47bedeaa18749d01711cddab0bc" = "sha256-DeSwiDyJeI9had5OCxLiGtYeDl07Vic0cR8RETBLY9k=";
      "https://github.com/input-output-hk/plutus.git"."983e6af2154c4bdf86ed645062bcb62f304d0a4f" = "sha256-Ga+hIhrgq2kR5Vnso/Edo2wgQpFn167eWGl35oM093U=";
      "https://github.com/Quid2/flat.git"."ee59880f47ab835dbd73bea0847dab7869fc20d8" = "lRFND+ZnZvAph6ZYkr9wl9VAx41pb3uSFP8Wc7idP9M=";
      "https://github.com/input-output-hk/cardano-crypto.git"."07397f0e50da97eaa0575d93bee7ac4b2b2576ec" = "oxIOVlgm07FAEmgGRF1C2me9TXqVxQulEOcJ22zpTRs=";
      "https://github.com/input-output-hk/cardano-base"."78b3928391b558fb1750228f63301ec371f13528" = "pBUTTcenaSLMovHKGsaddJ7Jh3okRTrtu5W7Rdu6RM4=";
      "https://github.com/input-output-hk/cardano-prelude"."fd773f7a58412131512b9f694ab95653ac430852" = "BtbT5UxOAADvQD4qTPNrGfnjQNgbYNO4EAJwH2ZsTQo=";
      "https://github.com/input-output-hk/Win32-network"."3825d3abf75f83f406c1f7161883c438dac7277d" = "Hesb5GXSx0IwKSIi42ofisVELcQNX6lwHcoZcbaDiqc=";
      "https://github.com/Srid/sydtest"."9c6c7678f7aabe22e075aab810a6a2e304591d24" = "sha256-P6ZwwfFeN8fpi3fziz9yERTn7BfxdE/j/OofUu+4GdA=";
      "https://github.com/Srid/autodocodec"."42b42a7407f33c6c74fa4e8c84906aebfed28daf" = "sha256-X1TNZlmO2qDFk3OL4Z1v/gzvd3ouoACAiMweutsYek4=";
      "https://github.com/Srid/validity"."f7982549b95d0ab727950dc876ca06b1862135ba" = "sha256-dpMIu08qXMzy8Kilk/2VWpuwIsfqFtpg/3mkwt5pdjA=";
    };
  };

  haskellNixFlake = project.flake { };

  tools = {
    onchain-ghcid-lib =
      ghcid { inherit subdir; name = "onchain-ghcid-lib"; args = "-c 'cabal repl'"; };
    onchain-ghcid-test =
      ghcid { inherit subdir; name = "onchain-ghcid-test"; args = "-c 'cabal repl test:tests'"; };
    offchain-ghcid-test-run =
      ghcid { inherit subdir; name = "onchain-ghcid-test-run"; args = "-c 'cabal repl test:tests' -T :main"; };
  };

  devShell = haskellNixFlake.devShell.overrideAttrs (oa: {
    shellHook = oa.shellHook + inputs.self.pseudoFlakes.${system}.flake-local.flakeLocal.shellHook;
    buildInputs = pkgs.lib.attrsets.attrValues (
      tools
    );
  });

  packages = haskellNixFlake.packages // { };

  checks = haskellNixFlake.checks // { };

  onchain-scripts = pkgs.stdenv.mkDerivation {
    name = "onchain-scripts";
    src = inputs.self; # FIXME: Why should src be project root here?
    buildInputs = [ haskellNixFlake.packages."dUSD-onchain:exe:scripts" ];
    doCheck = false;
    installPhase = ''
      scripts "$out"
    '';
    configurePhase = ''
      mkdir $out
    '';
  };
}
