{ lib, pkgs, config, ... }:
with lib;
let
  cfg = config.services.ctl-server;
in
{
  options.services.ctl-server = {

    enable = mkEnableOption "enable the ctl-server service";

    package = lib.mkOption {
      type = lib.types.package;
    };

    port = mkOption {
      type = types.port;
      default = 8081;
      example = 80;
      description = ''
        Port to listen on.
      '';
    };
  };

  config = mkIf cfg.enable {

    systemd.services.ctl-server =
      let
        args = escapeShellArgs [
          "--port"
          (toString cfg.port)
        ];
      in
      {
        description = "ctl-server";
        documentation = [ "https://github.com/Plutonomicon/cardano-transaction-lib" ];
        wantedBy = [ "multi-user.target" ];
        after = [ "cardano-ogmios.service" ];
        serviceConfig = mkMerge [
          {
            ExecStart = "${cfg.package}/bin/ctl-server ${args}";
            Restart = "always";
            DynamicUser = true;
          }
        ];
      };
  };
}
