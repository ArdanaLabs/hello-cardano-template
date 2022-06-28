{ self, ... }:
{
  perSystem = system: { config, self', inputs', ... }:
    let
      pkgs = inputs'.nixpkgs.legacyPackages;
      projectName = "hello-world";
      purs-nix = self.inputs.purs-nix-0-14 { inherit system; };
      npmlock2nix = pkgs.callPackages self.inputs.npmlock2nix {};

      ctl = ( purs-nix.build
              { name = "ctl";
                src.git={
                  repo = "https://github.com/Plutonomicon/cardano-transaction-lib.git";
                  ref = "develop";
                  rev = "472650f725ba77ab06b501be9a1b10a5fe72ee76";
                };
                info = {
                  version = "";
                  dependencies = with purs-nix.ps-pkgs;
                    [  aeson
                       aeson-helpers
                       aff
                       aff-promise
                       affjax
                       arraybuffer-types
                       arrays
                       bifunctors
                       bigints
                       checked-exceptions
                       console
                       const
                       control
                       debug
                       effect
                       either
                       encoding
                       enums
                       exceptions
                       foldable-traversable
                       foreign-object
                       http-methods
                       identity
                       integers
                       js-date
                       lattice
                       lists
                       maybe
                       medea
                       media-types
                       monad-logger
                       mote
                       newtype
                       node-buffer
                       node-fs
                       node-fs-aff
                       node-path
                       nonempty
                       ordered-collections
                       orders
                       partial
                       prelude
                       profunctor
                       profunctor-lenses
                       quickcheck
                       quickcheck-laws
                       quickcheck-combinators
                       rationals
                       record
                       refs
                       spec
                       spec-quickcheck
                       strings
                       tailrec
                       text-encoding
                       these
                       transformers
                       tuples
                       typelevel
                       typelevel-prelude
                       uint
                       undefined
                       unfoldable
                       untagged-union
                       variant
                    ];
                };
              }
      ) ;

      hello-world-api = {
        dependencies =
          with purs-nix.ps-pkgs;
            [ aeson
              aff
              bigints
              ctl
            ];
        ps =
          purs-nix.purs
            { inherit (hello-world-api) dependencies;
              srcs = [ ./hello-world-api/src ];
            };
        package = (purs-nix.build
          { name = "hello-world-api";
            src.path = ./hello-world-api;
            info = {
              inherit (hello-world-api) dependencies;
              version = "0.0.1";
            };
          });
      };

      hello-world-browser = {
        ps =
          purs-nix.purs
            { dependencies =
                with purs-nix.ps-pkgs;
                [ hello-world-api.package
                ];
              srcs = [ ./hello-world-browser/src ];
            };
      };

      hello-world-cli = {
        ps =
          purs-nix.purs
            { dependencies =
                with purs-nix.ps-pkgs;
                [ prelude
                  hello-world-api.package
                ];
              srcs = [ ./hello-world-cli/src ];
            };
      };

      ctl-pkgs = import self.inputs.nixpkgs {
                   inherit system;
                   overlays = [ self.inputs.cardano-transaction-lib.overlay.${system} ];
                 };
      # use more recent slot to avoid long sync time
      config = {
        datumCache.blockFetcher.firstBlock = {
          slot = 60854917;
          id = "1c157a01120772c35d468b425e6ef228d5b6cec5977f7897540aa8d0870f9ab9";
        };
      };
    in
    {
      packages.hello-world-api = hello-world-api.package;

      packages.hello-world-browser =
        pkgs.runCommand "build-hello-world-browser" { }
        # see buildPursProjcet: https://github.com/Plutonomicon/cardano-transaction-lib/blob/c906ead97563fef3b554320bb321afc31956a17e/nix/default.nix#L74
        # see bundlePursProject: https://github.com/Plutonomicon/cardano-transaction-lib/blob/c906ead97563fef3b554320bb321afc31956a17e/nix/default.nix#L149
        ''
        mkdir $out; cd $out
        export BROWSER_RUNTIME=1
        cp -r ${hello-world-browser.ps.modules.Main.output {}} output
        cp ${./hello-world-browser/index.js} index.js
        cp ${./hello-world-browser/index.html} index.html
        cp ${./webpack.config.js} webpack.config.js
        cp -r ${npmlock2nix.node_modules { src = ./.; }}/* .
        export NODE_PATH="node_modules"
        export PATH="bin:$PATH"
        mkdir dist
        webpack --mode=production -c webpack.config.js -o ./dist --entry ./index.js
        '';

      packages.hello-world-cli = hello-world-browser.ps.modules.Main.bundle {main = true;};

      apps = {
        ctl-runtime = ctl-pkgs.launchCtlRuntime config;

        serve-hello-world-browser = {
          type = "app";
          program = pkgs.writeShellApplication
            {
              name = projectName;
              runtimeInputs = [
                pkgs.nodePackages.http-server
              ];
              text = "http-server -c-1 ${self'.packages.hello-world-browser}";
            };
        };
      };

      devShells.hello-world-browser = pkgs.mkShell {
        name = projectName;
        buildInputs = (with pkgs; [
          nodejs-16_x
          (hello-world-browser.ps.command {})
          purs-nix.ps-pkgs.psci-support
          purs-nix.purescript
          purs-nix.purescript-language-server
          nodePackages.purs-tidy
        ]);
        shellHook = "export NODE_PATH=${npmlock2nix.node_modules { src = ./.; }}/node_modules/";
      };
    };
  flake = {
  };
}
