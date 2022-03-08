{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-21.11";
    flake-compat-ci.url = "github:hercules-ci/flake-compat-ci";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };
  outputs = { self, nixpkgs,  flake-compat, flake-compat-ci, }@inputs:
    let
      # System types to support.
      supportedSystems = [ "x86_64-linux" "aarch64-linux" ];

      # Helper function to generate an attrset '{ x86_64-linux = f "x86_64-linux"; ... }'.
      forAllSystems = nixpkgs.lib.genAttrs supportedSystems;

      # Nixpkgs instantiated for supported system types.
      nixpkgsFor = forAllSystems (system: import nixpkgs { inherit system; });

    in {
      devShell = forAllSystems (system: self.devShells.${system}.default);

      devShells = forAllSystems (system:
        let
          pkgs = nixpkgsFor."${system}";

          latexEnv = with pkgs; texlive.combine {
            inherit
              (texlive)
              scheme-basic
              latexmk
              ;
            };

        in rec
        {
          default = pkgs.mkShell {
            name = "dUSD";
            buildInputs = [
              latexEnv
              pkgs.entr
            ];
          };
        });

      apps = forAllSystems (system:
      let
        pkgs = nixpkgsFor."${system}";
      in
      {
        feedback-loop = {
          type = "app";
          program = "${pkgs.writeShellScript "feedback-loop" ''
            echo "test-plan.tex" | ${pkgs.entr}/bin/entr latexmk -pdf test-plan.tex
          ''}";
        };
      });
      ciNix = flake-compat-ci.lib.recurseIntoFlakeWith {
        flake = self;
        systems = [ "x86_64-linux" ];
      };
  };
}
