{ lib, pkgs, config, ... }:
let
  inherit (lib)
    mkIf
    mkEnableOption
    mdDoc
    ;
  cfg = config.services.ctl-runtime;
in
{
  options.services.ctl-runtime = {

    enable = mkEnableOption ''
      Enable the CTL runtime.
      This will enable the ogmios-datum-cache service and its required dependencies,
      as well as the ctl-server service.
    '';

  };

  config = mkIf cfg.enable {
    services.ogmios-datum-cache = {
      enable = true;
      postgresql = {
        user = "ogmios-datum-cache";
        dbName = "ogmios-datum-cache";
      };
    };

    services.ctl-server.enable = true;
  };
}
