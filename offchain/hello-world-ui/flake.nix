{
  description = "Hello World UI";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    purs-nix.url = "github:ursi/purs-nix";
  };

  outputs = { self, nixpkgs, ... }@inputs:
    let
      make-purs-nix = system:
        let
          purs-nix = inputs.purs-nix { inherit system; };
          inherit (purs-nix) ps-pkgs purs;
        in
        purs-nix
        // purs
             { dependencies =
                 with ps-pkgs;
                 [ aff
                   affjax
                   affjax-web
                   argonaut-codecs
                   argonaut-core
                   effect
                   either
                   foreign-object
                   halogen
                   http-methods
                   maybe
                   prelude
                   remotedata
                   transformers
                   tuples
                   type-equality
                 ];

               srcs = [ ./src ];
             };

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
          inherit (make-purs-nix system) modules;
        in

        {
          "${name}" =
            pkgs.runCommand name {}
              ''
              mkdir $out; cd $out
              ln -s ${modules.Main.bundle { esbuild.minify = true; }} main.js
              ln -s ${./dist/index.html} index.html
              '';
        });

      devShell = forAllSystems (system:
        let
          purs-nix = make-purs-nix system;

          pkgs = import nixpkgs {
            inherit system;
          };
        in
        pkgs.mkShell {
          inherit name;
          inputsFrom = builtins.attrValues self.packages.${system};
          buildInputs = (with pkgs; [
            nodejs-16_x
            (purs-nix.command {})
            purs-nix.esbuild
            purs-nix.purescript
            purs-nix.purescript-language-server
            purs-nix.purs-tidy
          ]);
        });
    };
}
