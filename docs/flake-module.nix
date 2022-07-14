{ self, ... }:
{
  perSystem = { config, self', inputs', system, ... }:
    let
      pkgs = inputs'.nixpkgs.legacyPackages;
    in
    {
      packages = {
        build-docs = pkgs.stdenv.mkDerivation {
          name = "build-docs";
          src = ./.;
          buildInputs = with pkgs; [ (texlive.combine { inherit (texlive) scheme-basic latexmk todonotes metafont; }) ];
          doCheck = false;
          buildPhase = ''
            HOME=$TMP latexmk -output-directory="tmp" -pdf ./*.tex
            mkdir $out -p
            cp tmp/*.pdf $out
          '';
          installPhase = ''
            ls -lah
          '';
        };
      };
      apps = {
        feedback-loop = {
          type = "app";
          program = pkgs.writeShellApplication
            {
              name = "dusd-feedback-loop";
              runtimeInputs = [ pkgs.entr ];
              text = ''
                find docs -name "*.tex" | entr nix build .#build-docs
              '';
            } + "/bin/dusd-feedback-loop";
        };
      };
    };
  flake = { };
}
