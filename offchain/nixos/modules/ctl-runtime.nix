{ lib, pkgs, config, ... }:
let
  inherit (lib)
    mkIf
    mkEnableOption
    ;
  cfg = config.services.ctl-runtime;
in
{
  options.services.ctl-runtime = {

    enable = mkEnableOption "enable the CTL runtime";

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
