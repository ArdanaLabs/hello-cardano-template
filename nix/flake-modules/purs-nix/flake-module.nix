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
        pkgs = mkOption {
          type = types.uniq (types.attrsOf types.unspecified);
          default =
            let
              f = self'':
                import ./ps-pkgs-ctl.nix {
                  ps-pkgs = config.ps.purs-nix.ps-pkgs // self'';
                  ctl-rev = self.inputs.cardano-transaction-lib.rev;
                };
              ps-pkgs-ctl =
                lib.fix
                  (self'':
                    builtins.mapAttrs
                      (n: v: config.ps.purs-nix.build (v // { name = n; }))
                      (f self'')
                  );
              all-ps-pkgs = config.ps.purs-nix.ps-pkgs // ps-pkgs-ctl;
            in
            all-ps-pkgs;
        };
      };
    };
  };
}

