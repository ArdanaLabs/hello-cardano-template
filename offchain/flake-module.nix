{ self, ... }:
{
  perSystem = { config, self', inputs', system, ... }:
    let
      pkgs = inputs'.nixpkgs.legacyPackages;
      projectName = "hello-world";
      purs-nix = self.inputs.purs-nix { inherit system; };
      npmlock2nix = pkgs.callPackages self.inputs.npmlock2nix { };

      ctl-rev = self.inputs.cardano-transaction-lib.rev;

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
            src.path = self'.packages.hello-world-cbor-purs;
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
                  halogen
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
    in
    {
      packages = {
        inherit hello-world-cbor;

        hello-world-api = hello-world-api.package;
        offchain-docs =
          pkgs.runCommand "offchain-docs" { }
            ''
              mkdir $out && cd $out
              # it may make sense to eventually add cli and browser to the srcs, but we need to not define Main twice
              ${hello-world-api.ps.command { srcs = [ ./hello-world-api/src ];} }/bin/purs-nix docs
            '';
        hello-world-browser =
          pkgs.runCommand "build-hello-world-browser" { }
            # see buildPursProjcet: https://github.com/Plutonomicon/cardano-transaction-lib/blob/c906ead97563fef3b554320bb321afc31956a17e/nix/default.nix#L74
            # see bundlePursProject: https://github.com/Plutonomicon/cardano-transaction-lib/blob/c906ead97563fef3b554320bb321afc31956a17e/nix/default.nix#L149
            ''
              mkdir $out && cd $out
              export BROWSER_RUNTIME=1
              cp -r ${hello-world-browser.ps.modules.Main.output {}} output
              cp ${./hello-world-browser/index.js} index.js
              cp ${./hello-world-browser/index.html} index.html
              cp ${./webpack.config.js} webpack.config.js
              cp -r ${ctlNodeModules}/* .
              export NODE_PATH="node_modules"
              export PATH="bin:$PATH"
              mkdir dist
              cp ${./hello-world-browser/main.css} dist/main.css
              webpack --mode=production -c webpack.config.js -o ./dist --entry ./index.js
            '';
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
        hello-world-api-tests =
          pkgs.runCommand "run-hello-world-api-tests" { NO_RUNTIME = "TRUE"; buildInputs = [ pkgs.coreutils ]; }
            ''
              mkdir $out && cd $out
              cp -r ${./hello-world-api/fixtures} fixtures
              ${self'.apps."offchain:hello-world-api:test".program} | tee test-report.txt
            '';
        hello-world-cli-tests =
          pkgs.runCommand "run-hello-world-cli-tests" { NO_RUNTIME = "TRUE"; buildInputs = [ pkgs.coreutils ]; }
            ''
              mkdir $out && cd $out
              cp -r ${./hello-world-cli/fixtures} fixtures
              ${self'.apps."offchain:hello-world-cli:test".program} | tee test-report.txt
            '';
      };

      apps =
        let
          mkApp = program: { type = "app"; inherit program; };
          makeServeApp = pathToServe:
            mkApp (
              pkgs.writeShellApplication
                {
                  name = projectName;
                  runtimeInputs = [ pkgs.nodePackages.http-server ];
                  text = "http-server -c-1 ${pathToServe}";
                }
            );
        in
        {
          ctl-runtime = ctl-pkgs.launchCtlRuntime config;

          serve-offchain-docs =
            makeServeApp "${self'.packages.offchain-docs}/generated-docs/html/";
          serve-hello-world-browser =
            makeServeApp self'.packages.hello-world-browser;

          "offchain:hello-world-api:test" =
            let
              meta.mainProgram = "hello-world-api-tests";
            in
            {
              type = "app";
              program =
                let
                  text = ''
                    export NODE_PATH=${ctlNodeModules}/node_modules
                    ${pkgs.nodejs}/bin/node \
                      --preserve-symlinks \
                      --input-type=module \
                      -e 'import { main } from "${hello-world-api.ps.modules."Test.Main".output {}}/Test.Main/index.js"; main()' \
                      -- "${ meta.mainProgram }" "''$@"
                  '';
                in
                pkgs.writeTextFile { inherit meta text; name = meta.mainProgram; executable = true; destination = "/bin/${meta.mainProgram}"; };
            };

          "offchain:hello-world-cli:test" =
            let
              meta.mainProgram = "hello-world-api-tests";
            in
            {
              type = "app";
              program =
                let
                  testExe = hello-world-cli.ps.modules."Test.Main".app { name = meta.mainProgram; version = "1.0.0"; };
                in
                pkgs.runCommand "wrap-${meta.mainProgram}" { inherit meta; buildInputs = [ pkgs.makeWrapper ]; } ''
                  mkdir -p $out/bin
                  makeWrapper ${testExe}/bin/${meta.mainProgram} $out/bin/${meta.mainProgram} \
                    --set PATH ${ pkgs.lib.makeBinPath [ self'.packages.hello-world-cli pkgs.coreutils ]}
                '';
            };
        };

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
        {
          hello-world-cli = makeProjectShell hello-world-cli { };
          hello-world-browser = makeProjectShell hello-world-browser { };
          hello-world-api = makeProjectShell hello-world-api { };
        };
    };
  flake = { };
}
