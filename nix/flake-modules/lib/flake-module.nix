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
      cat-lib = mkOption {
        type = types.uniq (types.attrsOf types.unspecified);
        description = ''
          Library functions. This is declared here
          so that utilizing the library is easier, instead of importing
          it everywhere needed (which also slows down evaluation, so this
          should be faster).
        '';
        default = import ../../lib {
          inherit self;
          realPkgs = inputs'.nixpkgs.legacyPackages;
        };
      };
    };
  };
}

