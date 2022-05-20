{ self, ... }:
{
  perSystem = system: { config, self', inputs', ... }:
    let
      pkgs = inputs'.nixpkgs.legacyPackages;
      projectName = "hello-world-ui";
      purs-nix = self.inputs.purs-nix { inherit system; };

      ps =
        purs-nix.purs
          { dependencies =
              with purs-nix.ps-pkgs;
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
    in
    {
      packages = {
        "${projectName}" = pkgs.runCommand projectName {}
          ''
          mkdir $out; cd $out
          ln -s ${ps.modules.Main.bundle { esbuild.minify = false; }} main.js
          ln -s ${./dist/index.html} index.html
          '';
      };
      apps = {
        ${projectName} = {
          type = "app";
          program = pkgs.writeShellApplication
            {
              name = projectName;
              runtimeInputs = [ pkgs.entr pkgs.nodePackages.http-server ];
              text =
                let
                  script = pkgs.writeScript "serve.sh" ''
                    nix build .#${projectName}
                    http-server -c-1 result
                  '';
                in
                ''
                  find . -name "*.purs" | entr -r ${script}
                '';
            } + "/bin/${projectName}";
        };
      };
      devShells.${projectName} = pkgs.mkShell {
        name = projectName;
        inputsFrom = builtins.attrValues self'.packages;
        buildInputs = (with pkgs; [
          nodejs-16_x
          (ps.command {})
          purs-nix.esbuild
          purs-nix.purescript
          purs-nix.purescript-language-server
          # purs-nix.purs-tidy
        ]);
      };
    };
  flake = {
  };
}
