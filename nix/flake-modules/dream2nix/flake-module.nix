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
          # we don't use types.attrsOf here because that will evaluate the attrs
          # which will evaluate `riseAndShine` in dream2nix, it is defined as a 
          # `throw "some-message"` which makes Nix throws an error immediately.
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
