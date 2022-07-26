{ self, ... }:
{
  perSystem = { config, self', inputs', system, ... }:
    let
      pkgs = inputs'.nixpkgs.legacyPackages;
      projectName = "hello-world";
      purs-nix = self.inputs.purs-nix { inherit system; };
      npmlock2nix = pkgs.callPackages self.inputs.npmlock2nix { };

      ctl-rev = self.inputs.cardano-transaction-lib.rev;

      dusd-lib = config.dusd-lib;

      ps-pkgs-ctl =
        let
          f = self:
            import ./ps-pkgs-ctl.nix {
              ps-pkgs = purs-nix.ps-pkgs // self;
              inherit ctl-rev;
            };
        in
        pkgs.lib.fix
          (self:
            builtins.mapAttrs (n: v: purs-nix.build (v // { name = n; })) (f self)
          );
      all-ps-pkgs = purs-nix.ps-pkgs // ps-pkgs-ctl;

      hello-world-cbor =
        purs-nix.build
          {
            name = "hello-world-cbor";
            src.path = self'.packages."onchain:hello-world-cbor-purs";
            info.dependencies = [ ];
            info.version = "0.0.1";
          };

      hello-world-api = {
        dependencies =
          with all-ps-pkgs;
          [
            aeson
            aff
            bigints
            cardano-transaction-lib
            hello-world-cbor
            ordered-collections
            spec
            node-process
          ];
        ps =
          purs-nix.purs
            {
              inherit (hello-world-api) dependencies;
              srcs = [ ./hello-world-api ];
            };
        package =
          purs-nix.build
            {
              name = "hello-world-api";
              src.path = ./hello-world-api;
              info = {
                inherit (hello-world-api) dependencies;
                version = "0.0.1";
              };
            };
      };

      hello-world-browser = {
        ps =
          purs-nix.purs
            {
              dependencies =
                with all-ps-pkgs;
                [
                  aff
                  bigints
                  halogen
                  halogen-store
                  safe-coerce
                  transformers
                  cardano-transaction-lib
                  hello-world-api.package
                ];
              srcs = [ ./hello-world-browser ];
            };
      };

      hello-world-cli = {
        ps =
          purs-nix.purs
            {
              dependencies =
                with all-ps-pkgs;
                [
                  prelude
                  hello-world-api.package
                  optparse
                  node-fs-aff
                  node-fs
                  dotenv
                  node-child-process
                  stringutils
                ];
              srcs = [ ./hello-world-cli ];
            };
      };

      ctlNodeModules = "${npmlock2nix.node_modules { src = self.inputs.cardano-transaction-lib; }}";

      ctl-pkgs = import self.inputs.nixpkgs {
        inherit system;
        overlays = [ self.inputs.cardano-transaction-lib.overlay ];
      };

      # use more recent slot to avoid long sync time
      ctlRuntimeConfig = {
        datumCache.blockFetcher.firstBlock = {
          slot = 62153233;
          id = "631c621b7372445acf82110282ba72f4b52dafa09c53864ddc2e58be24955b2a";
        };
      };

      hello-world-api-tests =
        let
          testModule = hello-world-api.ps.modules."Test.Main".output { };
          scriptName = "hello-world-api-tests";
        in
        pkgs.writeShellApplication
          {
            name = scriptName;
            runtimeInputs = [ pkgs.nodejs ];
            text = ''
              export TEST_RESOURCES=${./hello-world-api/fixtures}
              export NODE_PATH=${ctlNodeModules}/node_modules
              node \
                --preserve-symlinks \
                --input-type=module \
                -e 'import { main } from "${testModule}/Test.Main/index.js"; main()' \
                -- "${scriptName}" "''$@"
            '';
          };
      hello-world-cli-tests =
        let
          testExe =
            hello-world-cli.ps.modules."Test.Main".app
              { name = scriptName; };
          scriptName = "hello-world-cli-tests";
        in
        pkgs.writeShellApplication
          {
            name = scriptName;
            runtimeInputs = [
              testExe
              self'.packages."offchain:hello-world-cli"
              pkgs.coreutils
            ];
            text = ''
              export TEST_RESOURCES=${./hello-world-cli/fixtures}
              ${scriptName}
            '';
          };
      prefixOutputs = dusd-lib.prefixAttrNames "offchain";
    in
    {
      packages =
        let
          make-hello-world-browser-package = { indexJs }:
            pkgs.runCommand "build-hello-world-browser" { }
              # see buildPursProject: https://github.com/Plutonomicon/cardano-transaction-lib/blob/c906ead97563fef3b554320bb321afc31956a17e/nix/default.nix#L74
              # see bundlePursProject: https://github.com/Plutonomicon/cardano-transaction-lib/blob/c906ead97563fef3b554320bb321afc31956a17e/nix/default.nix#L149
              ''
                mkdir $out && cd $out
                export BROWSER_RUNTIME=1
                cp -r ${hello-world-browser.ps.modules.Main.output {}} output
                cp ${indexJs} index.js
                cp ${./hello-world-browser/index.html} index.html
                cp ${./webpack.config.js} webpack.config.js
                cp -r ${ctlNodeModules}/* .
                export NODE_PATH="node_modules"
                export PATH="bin:$PATH"
                mkdir dist
                cp ${./hello-world-browser/main.css} dist/main.css
                webpack --mode=production -c webpack.config.js -o ./dist --entry ./index.js
              '';
        in
        prefixOutputs {
          inherit hello-world-cbor;
          hello-world-api = hello-world-api.package;
          docs =
            pkgs.runCommand "offchain-docs" { }
              ''
                mkdir $out && cd $out
                # it may make sense to eventually add cli and browser to the srcs, but we need to not define Main twice
                ${hello-world-api.ps.command { srcs = [ ./hello-world-api/src ];} }/bin/purs-nix docs
              '';
          hello-world-browser =
            make-hello-world-browser-package
              { indexJs = ./hello-world-browser/index.js; };
          hello-world-browser-for-testing =
            make-hello-world-browser-package
              { indexJs = ./hello-world-browser/test.js; };
          hello-world-cli =
            let js = "${hello-world-cli.ps.modules.Main.output {}}/Main/index.js"; in
            pkgs.writeScriptBin "hello-world-cli"
              ''
                export NODE_PATH=${ctlNodeModules}/node_modules
                ${pkgs.nodejs}/bin/node \
                  --preserve-symlinks \
                  --input-type=module \
                  -e 'import { main } from "${js}"; main()' \
                  -- "hello-world-cli" "''$@"
              '';
        };

      checks = {
        run-hello-world-api-tests =
          let test = hello-world-api-tests; in
          pkgs.runCommand test.name { NO_RUNTIME = "TRUE"; }
            "${test}/bin/${test.meta.mainProgram} | tee $out";
        run-hello-world-cli-tests =
          let test = hello-world-cli-tests; in
          pkgs.runCommand test.name { NO_RUNTIME = "TRUE"; }
            "${test}/bin/${test.meta.mainProgram} | tee $out";
      };

      apps =
        { ctl-runtime = ctl-pkgs.launchCtlRuntime config; }
        // (
          let
            makeServeApp = pathToServe:
              dusd-lib.mkApp (
                pkgs.writeShellApplication
                  {
                    name = projectName;
                    runtimeInputs = [ pkgs.nodePackages.http-server ];
                    text = "http-server -c-1 ${pathToServe}";
                  }
              );
          in
          prefixOutputs {
            "docs:serve" =
              makeServeApp "${self'.packages."offchain:docs"}/generated-docs/html/";
            "hello-world-browser:serve" =
              makeServeApp self'.packages."offchain:hello-world-browser";

            "hello-world-api:test" =
              dusd-lib.mkApp hello-world-api-tests;
            "hello-world-cli:test" =
              dusd-lib.mkApp hello-world-cli-tests;
          }
        );

      devShells =
        let
          # Helper function to create a devshell without declaring common dependencies.
          # If you want to add more dependencies, use `.overrideAttrs (old: { ... })`.
          makeProjectShell = project: cmdArgs:
            pkgs.mkShell {
              name = projectName;
              buildInputs = (with pkgs; [
                nodejs-16_x
                (project.ps.command cmdArgs)
                purs-nix.ps-pkgs.psci-support
                purs-nix.purescript
                purs-nix.purescript-language-server
                nodePackages.purs-tidy
              ]);
              shellHook = "export NODE_PATH=${ctlNodeModules}/node_modules/";
            };
        in
        prefixOutputs {
          hello-world-cli = makeProjectShell hello-world-cli { };
          hello-world-browser = makeProjectShell hello-world-browser { };
          hello-world-api = makeProjectShell hello-world-api { };
        };
    };
  flake = { };
}
