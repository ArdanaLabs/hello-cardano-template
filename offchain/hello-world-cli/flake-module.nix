{ self, ... }:
{
  perSystem = { config, self', inputs', system, ... }:
    let
      pkgs = inputs'.nixpkgs.legacyPackages;
      purs-nix = config.ps.purs-nix;
      inherit (purs-nix) ps-pkgs;
      inherit (config) dusd-lib offchain-lib;

      hello-world-cli = {
        ps =
          purs-nix.purs
            {
              dependencies =
                with ps-pkgs;
                [
                  prelude
                  optparse
                  node-fs-aff
                  node-fs
                  dotenv
                  stringutils
                  bigints
                  self'.packages."offchain:hello-world-api"
                ];
              dir = ./.;
            };
        package =
          let js = "${hello-world-cli.ps.modules.Main.output {}}/Main/index.js"; in
          pkgs.writeScriptBin "hello-world-cli"
            ''
              export NODE_PATH=${config.ctl.nodeModules}/node_modules
              ${pkgs.nodejs}/bin/node \
                --preserve-symlinks \
                --input-type=module \
                -e 'import { main } from "${js}"; main()' \
                -- "hello-world-cli" "''$@"
            '';
      };

      hello-world-cli-tests =
        let testExe = hello-world-cli.ps.test.run { }; in
        pkgs.writeShellApplication
          {
            name = "hello-world-cli-tests";
            runtimeInputs = [
              pkgs.coreutils
              pkgs.postgresql
              self'.packages."offchain:hello-world-cli"
              testExe
            ];
            text = ''
              export LC_ALL=C.utf-8
              # this fixes a postgresql issue for me (Brian)
              # I think this is related https://github.com/NixOS/nixpkgs/issues/60414
              export NODE_PATH=${config.ctl.nodeModules}/node_modules
              export TEST_RESOURCES=${./fixtures}
              ${testExe}
            '';
          };
    in
    {
      apps."offchain:hello-world-cli:test" = dusd-lib.mkApp hello-world-cli-tests;
      checks.run-hello-world-cli-tests =
        let test = hello-world-cli-tests; in
        pkgs.runCommand test.name { }
          "${test}/bin/${test.meta.mainProgram} | tee $out";
      devShells."offchain:hello-world-cli" =
        offchain-lib.makeProjectShell hello-world-cli { };
      packages."offchain:hello-world-cli" = hello-world-cli.package;
    };
  flake = { };
}
