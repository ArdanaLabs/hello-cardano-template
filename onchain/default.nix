{ system, pkgs, self, plutus, ... }:

let
  inherit (pkgs.callPackage ../nix/plutus.nix { inherit system pkgs self plutus; }) 
    plutusProjectIn;
in rec {
  project = plutusProjectIn {
    subdir = "onchain";
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
    pkg-def-extras = [];
    sha256map = {
      "https://github.com/mlabs-haskell/apropos"."455b1a3ad1eee35de4d3fb8c4a4527071474336c" = "sha256-EC6vnimXA+jBRPQLLs2dltuTx9XoSdkQfh742NnLJSQ=";
      "https://github.com/mlabs-haskell/apropos-tx"."489eeb8c30d62d5c75eafe4242a1f133695f8564" = "sha256-15nFGPhXBy+G0oocb6KQf5KVnT0fuAOoFCdzT+vyeEg=";
      "https://github.com/Plutonomicon/plutarch"."cae000584f5492daa2e13a08632ea69221a8c147" = "sha256-ZEGKBr1q852sx5aOOAqWHKZR+//YByQmGmBKglGr+qQ=";
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
  flake = project.flake { };
}