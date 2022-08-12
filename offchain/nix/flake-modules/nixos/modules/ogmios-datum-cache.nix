{ lib, pkgs, config, ... }:
let
  inherit (lib)
    types
    mkOption
    mkIf
    mkMerge
    mkEnableOption
    ;
  cfg = config.services.ogmios-datum-cache;
in
{
  options.services.ogmios-datum-cache = {

    enable = mkEnableOption "enable the ogmios-datum-cache service";

    package = mkOption {
      type = types.package;
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
            The first blocks id hash.
          '';
        };
      };

      filter = mkOption {
        type = types.anything;
        default =
          {
            const = true;
          };
        description = ''
          A filter description in JSON format.
        '';
      };
    };

    postgresql = {
      host = mkOption {
        type = types.str;
        default = "/run/postgresql";
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

      dbName = mkOption {
        type = types.str;
        default = "ogmios-datum-cache";
        description = ''
          The postgresql database name.
        '';
      };
    };
  };

  config = mkIf cfg.enable {

    users.users.ogmios-datum-cache = {
      name = "ogmios-datum-cache";
      group = "ogmios-datum-cache";
      isSystemUser = true;
      useDefaultShell = true;
    };

    users.groups.ogmios-datum-cache = { };

    systemd.services.ogmios-datum-cache =
      let
        args = lib.escapeShellArgs [
          "--log-level"
          "warn"
          "--use-latest"
          "--server-api"
          cfg.controlApiToken
          "--server-port"
          (toString cfg.port)
          "--ogmios-address"
          config.services.cardano-ogmios.hostAddr
          "--ogmios-port"
          (toString config.services.cardano-ogmios.port)
          "--db-host"
          cfg.postgresql.host
          "--db-port"
          (toString config.services.postgresql.port)
          "--db-user"
          cfg.postgresql.user
          "--db-name"
          cfg.postgresql.dbName
          "--block-slot"
          (toString cfg.blockFetcher.firstBlock.slot)
          "--block-hash"
          cfg.blockFetcher.firstBlock.blockHash
          "--block-filter"
          (lib.strings.replaceStrings [ "\"" "\\" ] [ "\\\"" "\\\\" ] (builtins.toJSON cfg.blockFetcher.filter))
        ];
      in
      {
        description = "ogmios-datum-cache";
        documentation = [ "https://github.com/mlabs-haskell/ogmios-datum-cache" ];
        wantedBy = [ "multi-user.target" ];
        after = [ "cardano-ogmios.service" "networking.target" "postgresql.service" ];
        serviceConfig = mkMerge [
          {
            ExecStart = "${cfg.package}/bin/ogmios-datum-cache ${args}";
            Restart = "always";
            User = "ogmios-datum-cache";
            Group = "ogmios-datum-cache";
          }
        ];
      };
  };
}
