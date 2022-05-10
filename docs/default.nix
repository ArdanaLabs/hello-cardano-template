{ pkgs, ... }:

pkgs.stdenv.mkDerivation {
  name = "build-docs";
  src = ./.;
  buildInputs = with pkgs; [ (texlive.combine { inherit ( texlive ) scheme-basic latexmk todonotes metafont; }) ];
  doCheck = false;
  buildPhase = ''
    HOME=$TMP latexmk -output-directory="tmp" -pdf ./*.tex
    mkdir $out -p
    cp tmp/*.pdf $out
  '';
  installPhase = ''
    ls -lah
  '';
}