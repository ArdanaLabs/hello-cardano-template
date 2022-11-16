{ lib, pkgs, config, ... }:
let
  inherit (lib)
    mkIf
    mkEnableOption
    mdDoc
    mkOption
    types
    escapeShellArgs
    mkMerge
    ;
  cfg = config.services.ctl-server;
in
{
  options.services.ctl-server = {

    enable = mkEnableOption (mdDoc "ctl-server service");

    package = mkOption {
      type = types.package;
    };

    port = mkOption {
      type = types.port;
      default = 8081;
      example = 8081;
      description = mdDoc ''
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
