{
  description = "Hello World UI";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    easy-purescript-nix = {
      url = "github:justinwoo/easy-purescript-nix?rev=aa72388ca0fb72ed64467f59a121db1f104897db";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, easy-purescript-nix, ... }@inputs:
    let
      name = "hello-world-ui";

      supportedSystems = [
        "x86_64-linux"
      ];

      forAllSystems = f: nixpkgs.lib.genAttrs supportedSystems (system: f system);
    in
    {
      defaultApp = forAllSystems (system: self.apps.${system}.${name});

      apps = forAllSystems (system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
        in
          {
            "${name}" = {
              type = "app";
              program = (pkgs.writeShellApplication {
                inherit name;
                text = ''
                  cd ./.
                  ${pkgs.nodePackages.serve}/bin/serve ${self.defaultPackage.${system}}
                '';
              }) + "/bin/${name}";
            };
          });

      defaultPackage = forAllSystems (system: self.packages.${system}.${name});

      packages = forAllSystems (system:
        let
          pkgs = import nixpkgs { inherit system; };

          easy-ps = import easy-purescript-nix { inherit pkgs; };

          spagoPkgs = import ./spago-packages.nix { inherit pkgs; };
        in

        {
          "${name}" = pkgs.stdenv.mkDerivation {
            inherit name;
            src = ./.;
            buildInputs = with spagoPkgs; [ installSpagoStyle buildSpagoStyle ];
            nativeBuildInputs = with easy-ps; [ purs spago zephyr pkgs.esbuild ];
            unpackPhase = ''
              cp -R $src/* .
              install-spago-style
            '';
            buildPhase = ''
              build-spago-style "./src/**/*.purs" --codegen corefn
              zephyr -f Main.main
              esbuild --external:url --external:xhr2 --outfile=main.js --bundle index.js --minify
            '';
            installPhase = ''
              mkdir $out
              mv main.js $out/
              cp dist/index.html $out/
            '';
          };
        });

      devShell = forAllSystems (system:
        let
          pkgs = import nixpkgs {
            inherit system;
          };

          easy-ps = import easy-purescript-nix { inherit pkgs; };
        in
        pkgs.mkShell {
          inherit name;
          inputsFrom = builtins.attrValues self.packages.${system};
          buildInputs = (with pkgs; [
            dhall
            dhall-json
            nodejs-16_x
          ]) ++ (with easy-ps; [
            purescript-language-server
            purs-tidy
            spago
            spago2nix
            zephyr
          ]);
        });
    };
}
