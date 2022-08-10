{ self, lib, ... }:
let
  inherit (lib)
    mkOption
    types
    ;
in
{
  perSystem = { inputs', ... }: {
    options = {
      dream2nix = {
        lib = mkOption {
          # we don't use types.attrsOf here because it will throw
          # an error related to `riseAndShine`, which is in dream2nix
          # for compat purposes
          type = types.unspecified;
          description = "dream2nix instance";
          default =
            self.inputs.dream2nix.lib.init
              {
                pkgs = inputs'.nixpkgs.legacyPackages;
                config = {
                  overridesDirs = [
                    "${self.inputs.dream2nix}/overrides"
                    "${self}/nix/dream2nix-overrides"
                  ];
                  projectRoot = self;
                };
              };
        };
      };
    };
  };
}
