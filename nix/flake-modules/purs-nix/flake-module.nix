{ lib, self, ... }:
let
  inherit (lib)
    mkOption
    types
    ;
in
{
  perSystem = { config, self', inputs', system, ... }: {
    options = {
      ps = {
        purs-nix = mkOption {
          type = types.unspecified;
          default = self.inputs.purs-nix { inherit system; };
        };
        ctl-pkgs = mkOption {
          type = types.uniq (types.attrsOf types.unspecified);
          default =
            let inherit (config.ps) purs-nix; in
            purs-nix.build-set
              (import ./ps-pkgs-ctl.nix {
                inherit (purs-nix) ps-pkgs;
                ctl-rev = self.inputs.cardano-transaction-lib.rev;
              });
        };
      };
    };
  };
}

