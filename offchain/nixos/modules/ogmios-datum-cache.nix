{ lib, pkgs, config, ... }:
with lib;
let
  cfg = config.services.ogmios-datum-cache;
in {
  options.services.ogmios-datum-cache = {

    enable = mkEnableOption "enable the ogmios-datum-cache service";

    package = lib.mkOption {
      type = lib.types.package;
      default = pkgs.ogmios-datum-cache;
    };

    host = mkOption {
      type = types.str;
      default = "127.0.0.1";
      description = ''
        Address to listen on.
      '';
    };

    port = mkOption {
      type = types.port;
      default = 9999;
      example = 80;
      description = ''
        Port to listen on.
      '';
    };

    controlApiToken = mkOption {
      type = types.str;
      default = "";
      description = "Control Api Token";
    };

    blockFetcher = {
      firstBlock = {
        slot = mkOption {
          type = types.ints.positive;
          default = 61625527;
          description = ''
            The first blocks slot.
          '';
        };

        blockHash = mkOption {
          type = types.str;
          default = "3afd8895c7b270f8250b744ec8d2b3c53ee2859c9d5711d906c47fe51b800988";
          description = ''
            The first blocks id.
          '';  
        };
      };

      autoStart = mkOption {
        type = types.bool;
        default = true;
        description = ''
          Whether the fetching of blocks should be started automatically.
        '';
      };

      startFromLast = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Whether it should start from the last block.
        '';
      };

      filter = mkOption {
        type = types.str;
        default = ''
        {
          "const": true;
        }
        '';
        description = ''
          A filter description in JSON format.
        '';
      };
    };

    postgresql = {
      host = mkOption {
        type = types.str;
        default = "127.0.0.1";
        description = ''
          Address to listen on.
        '';
      };

      user = mkOption {
        type = types.str;
        default = "ogmios-datum-cache";
        description = ''
          The postgresql database user.
        '';
      };

      password = mkOption {
        type = types.str;
        default = "ogmios-datum-cache";
        description = ''
          The postgresql users password.
        '';
      };
    };

    stateDir = mkOption {
      type = types.path;
      default = "/var/lib/ogmios-datum-cache";
      description = ''
        Path the service has access to. If left as the default value this
        directory will automatically be created before the ogmios-datum-cache server
        starts, otherwise the sysadmin is responsible for ensuring the
        directory exists with appropriate ownership and permissions.
      '';
    };
  };

  config = mkIf cfg.enable {

    users.users.ogmios-datum-cache = {
      name = "ogmios-datum-cache";
      group = "ogmios-datum-cache";
      isSystemUser = true;
      home = cfg.stateDir;
      useDefaultShell = true;
    };

    users.groups.ogmios-datum-cache = {};

    systemd.services.ogmios-datum-cache =
      let
        args = escapeShellArgs [
          "--log-level warn"
          "--use-latest"
          "--server-api '${cfg.controlApiToken}'"
          "--server-port ${toString cfg.port}"
          "--ogmios-address ${config.services.cardano-ogmios.hostAddr}"
          "--ogmios-port ${toString config.services.cardano-ogmios.port}"
          "--db-port ${toString config.services.postgresql.port}"
          "--db-host ${cfg.postgresql.host}"
          "--db-user ${cfg.postgresql.user}"
          "--db-password ${cfg.postgresql.password}"
          "--block-slot ${toString cfg.blockFetcher.firstBlock.slot}"
          "--block-hash '${cfg.blockFetcher.firstBlock.blockHash}'"
          "--block-filter '${cfg.blockFetcher.filter}'"
        ];
      in {
        description = "ogmios-datum-cache";
        documentation = [ "https://github.com/mlabs-haskell/ogmios-datum-cache" ];
        wantedBy = [ "multi-user.target" ];
        after = [ "cardano-ogmios.target" "networking.target" "postgresql.target" ];
        serviceConfig = mkMerge [
          {
            ExecStart = "${cfg.package}/bin/ogmios-datum-cache ${args}";
            Restart = "always";
            WorkingDirectory = cfg.stateDir;
            User = "ogmios-datum-cache";
            Group = "ogmios-datum-cache";
            PrivateTmp = true;
          }
          (mkIf (cfg.stateDir == "/var/lib/ogmios-datum-cache") {
            StateDirectory = "ogmios-datum-cache";
          })
        ];
      };
  };
}
