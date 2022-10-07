{ self, lib, ... }:
let
  inherit (lib)
    types
    mkOption
    ;
in
{
  perSystem = { config, self', inputs', ... }:
    let
      pkgs = inputs'.nixpkgs.legacyPackages;
      cat-lib = config.cat-lib;
      ps-tools = inputs'.ps-tools.legacyPackages;
      purs-nix = config.ps.purs-nix;
    in
    {
      options = {
        # These are some utilities we will use often in offchain nix code.
        offchain-lib = {
          makeProjectShell = mkOption {
            type = types.functionTo types.package;
            description = ''
              Helper function to create a devshell without declaring common dependencies.
              If you want to add more dependencies, use `.overrideAttrs (old: { ... })`.
            '';
            default = { project, cmdArgs ? { }, extraBuildInputs ? [ ] }:
              pkgs.mkShell {
                name = "hello-world";
                buildInputs = (with pkgs; [
                  nodejs-16_x
                  (project.ps.command cmdArgs)
                  ps-tools.for-0_14.purescript-language-server
                  ps-tools.for-0_14.purs-tidy
                  purs-nix.purescript
                  self.inputs.cardano-transaction-lib.inputs.plutip.packages.${pkgs.system}."plutip:exe:plutip-server"
                  self.inputs.cardano-transaction-lib.packages.${pkgs.system}."ctl-server:exe:ctl-server"
                  self.inputs.mlabs-ogmios.defaultPackage.${pkgs.system}
                  self.inputs.ogmios-datum-cache.defaultPackage.${pkgs.system}
                ]) ++ extraBuildInputs;
                shellHook = "export NODE_PATH=${config.ctl.nodeModules}/node_modules/";
              };
          };
        };
      };
    };
}
