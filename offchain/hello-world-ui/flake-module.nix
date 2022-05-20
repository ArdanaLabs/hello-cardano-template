{ self, ... }:
{
  perSystem = system: { config, self', inputs', ... }:
  let
    make-purs-nix = system:
      let
        purs-nix = self.inputs.purs-nix { inherit system; };
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
      pkgs = inputs'.nixpkgs.legacyPackages;
      projectName = "hello-world-ui";
      inherit (make-purs-nix system) modules;
      purs-nix = make-purs-nix system;
    in
    {
      packages = {
        "${projectName}" = pkgs.runCommand projectName {}
          ''
          mkdir $out; cd $out
          ln -s ${modules.Main.bundle { esbuild.minify = true; }} main.js
          ln -s ${./dist/index.html} index.html
          '';
      };
      apps = {
        ${projectName} = {
          type = "app";
          program = pkgs.writeShellApplication
            {
              name = projectName;
              runtimeInputs = [ pkgs.entr pkgs.nodePackages.serve ];
              text = ''
                find . -name "*.purs" | entr -r sh -c 'nix build .#hello-world-ui && serve result'
              '';
            } + "/bin/${projectName}";
        };
      };
      devShells.${projectName} = pkgs.mkShell {
        name = projectName;
        inputsFrom = builtins.attrValues self'.packages;
        buildInputs = (with pkgs; [
          nodejs-16_x
          (purs-nix.command {})
          purs-nix.esbuild
          purs-nix.purescript
          purs-nix.purescript-language-server
          purs-nix.purs-tidy
        ]);
      };
    };
  flake = {
  };
}
