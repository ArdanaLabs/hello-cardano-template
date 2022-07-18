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

      hello-world-cbor = purs-nix.build
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
          ];
        ps =
          purs-nix.purs
            {
              inherit (hello-world-api) dependencies;
              srcs = [ ./hello-world-api/src ];
            };
        package = (purs-nix.build
          {
            name = "hello-world-api";
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
            {
              dependencies =
                with all-ps-pkgs;
                [
                  halogen
                  cardano-transaction-lib
                  hello-world-api.package
                ];
              srcs = [ ./hello-world-browser/src ];
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
                ];
              srcs = [ ./hello-world-cli/src ];
            };
      };

      ctl-pkgs = import self.inputs.nixpkgs {
        inherit system;
        overlays = [ self.inputs.cardano-transaction-lib.overlay ];
      };
      # use more recent slot to avoid long sync time
      config = {
        datumCache.blockFetcher.firstBlock = {
          slot = 63181679;
          id = "3697256b3c1b594a39c2cc50daf15df0c0d9b15e2e07d7c44987c26b5aa099a2";
        };
      };
    in
    {
      packages.hello-world-cbor = hello-world-cbor;

      packages.hello-world-api = hello-world-api.package;

      packages.hello-world-browser =
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
            cp -r ${npmlock2nix.node_modules { src = self.inputs.cardano-transaction-lib ; }}/* .
            export NODE_PATH="node_modules"
            export PATH="bin:$PATH"
            mkdir dist
            cp ${./hello-world-browser/main.css} dist/main.css
            webpack --mode=production -c webpack.config.js -o ./dist --entry ./index.js
          '';

      packages.hello-world-cli =
        let js = "${hello-world-cli.ps.modules.Main.output {}}/Main/index.js"; in
        pkgs.writeScriptBin "hello-world-cli"
          ''
            export NODE_PATH=${npmlock2nix.node_modules { src = self.inputs.cardano-transaction-lib; }}/node_modules
            ${hello-world-cli.ps.command {srcs = [ ./hello-world-cli ];}}/bin/purs-nix run $@
          '';

      checks.hello-world-api-tests = pkgs.runCommand
        "api-tests"
        { NODE_PATH = "${npmlock2nix.node_modules { src = self.inputs.cardano-transaction-lib; }}/node_modules"; }
        ''
          mkdir $out && cd $out
          ${hello-world-api.ps.command {srcs = [ ./hello-world-api ];}}/bin/purs-nix test
        '';

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

      devShells.hello-world-cli = pkgs.mkShell {
        name = projectName;
        buildInputs = (with pkgs; [
          nodejs-16_x
          (hello-world-cli.ps.command { })
          purs-nix.ps-pkgs.psci-support
          purs-nix.purescript
          purs-nix.purescript-language-server
          nodePackages.purs-tidy
        ]);
        shellHook = "export NODE_PATH=${npmlock2nix.node_modules { src = self.inputs.cardano-transaction-lib ; }}/node_modules/";
      };
      devShells.hello-world-browser = pkgs.mkShell {
        name = projectName;
        buildInputs = (with pkgs; [
          nodejs-16_x
          (hello-world-browser.ps.command { })
          purs-nix.ps-pkgs.psci-support
          purs-nix.purescript
          purs-nix.purescript-language-server
          nodePackages.purs-tidy
        ]);
        shellHook = "export NODE_PATH=${npmlock2nix.node_modules { src = self.inputs.cardano-transaction-lib; }}/node_modules/";
      };
      devShells.hello-world-api = pkgs.mkShell {
        name = projectName;
        buildInputs = (with pkgs; [
          nodejs-16_x
          (hello-world-api.ps.command { })
          purs-nix.ps-pkgs.psci-support
          purs-nix.purescript
          purs-nix.purescript-language-server
          nodePackages.purs-tidy
        ]);
        shellHook = "export NODE_PATH=${npmlock2nix.node_modules { src = self.inputs.cardano-transaction-lib; }}/node_modules/";
      };
    };
  flake = { };
}
