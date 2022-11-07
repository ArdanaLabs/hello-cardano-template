{ lib, pkgs, config, ... }:
let
  inherit (lib)
    types
    mkOption
    mkIf
    mkMerge
    mkEnableOption
    mdDoc
    ;
  cfg = config.services.ogmios-datum-cache;
in
{
  options.services.ogmios-datum-cache = {

    enable = mkEnableOption ''
      Enable the ogmios-datum-cache service.
      This will enable a postgres-database service, cardano-ogmios and cardano-node.
    '';

    package = mkOption {
      type = types.package;
    };

    port = mkOption {
      type = types.port;
      default = 8889;
      description = mdDoc ''
        Port to listen on.
      '';
    };

    controlApiToken = mkOption {
      type = types.str;
      default = "";
      description = mdDoc ''
        Defines the secrete token, required for control API call. Format: user:password
        See: https://github.com/mlabs-haskell/ogmios-datum-cache/blob/master/README.md
      '';
    };

    blockFetcher = {
      firstBlock = mkOption {
        type = types.nullOr (types.submodule {
          options = {
            slot = mkOption {
              type = types.ints.positive;
              description = mdDoc ''
                Slot of first block to fetch by initial block fetcher.
                See: https://github.com/mlabs-haskell/ogmios-datum-cache/blob/master/README.md
              '';
            };

            blockHash = mkOption {
              type = types.str;
              description = mdDoc ''
                Hash of block's HEADER not hash of a block itself.
                See: https://github.com/mlabs-haskell/ogmios-datum-cache/blob/master/README.md
              '';
            };
          };
        });
        default = {
          slot = 7984046;
          blockHash = "b353d8b6ec01692a9f2b180e0fcb84b015eac267a581065f223e0033566b3dcb";
        };
        description = mdDoc ''
          Optionally set the first block from which you want the block-fetcher to start.
          If not set, ODC will start from the chain tip.
          See: https://github.com/mlabs-haskell/ogmios-datum-cache/blob/master/README.md
        '';
      };

      filter = mkOption {
        type = types.anything;
        default =
          {
            const = true;
          };
        description = mdDoc ''
          A filter description in JSON format.
        '';
      };
    };

    postgresql = {
      host = mkOption {
        type = types.str;
        default = "/run/postgresql";
        description = mdDoc ''
          The postgresql service host address.
        '';
      };

      user = mkOption {
        type = types.str;
        default = "ogmios-datum-cache";
        description = mdDoc ''
          The postgresql database user for ogmios-datum-cache.
        '';
      };

      dbName = mkOption {
        type = types.str;
        default = "ogmios-datum-cache";
        description = mdDoc ''
          The postgresql database name for ogmios-datum-cache.
        '';
      };
    };
  };

  config = mkIf cfg.enable {

    users.users.ogmios-datum-cache = {
      name = "ogmios-datum-cache";
      group = "ogmios-datum-cache";
      # we need ODC to have a system user because of the way we authenticate with postgresql
      isSystemUser = true;
    };

    users.groups.ogmios-datum-cache = { };

    systemd.services.ogmios-datum-cache =
      let
        args = lib.escapeShellArgs ([
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
          "--block-filter"
          (lib.strings.replaceStrings [ "\"" "\\" ] [ "\\\"" "\\\\" ] (builtins.toJSON cfg.blockFetcher.filter))
        ] ++
        (if builtins.isNull cfg.blockFetcher.firstBlock
        then [ "--from-tip" ]
        else [
          "--block-hash"
          cfg.blockFetcher.firstBlock.blockHash
          "--block-slot"
          (toString cfg.blockFetcher.firstBlock.slot)
        ]
        ));
      in
      {
        description = "ogmios-datum-cache";
        documentation = [ "https://github.com/mlabs-haskell/ogmios-datum-cache" ];
        wantedBy = [ "multi-user.target" ];
        after = [ "networking.target" "cardano-node.service" "cardano-ogmios.service" "postgresql.service" ];
        serviceConfig = mkMerge [
          {
            ExecStart = "${cfg.package}/bin/ogmios-datum-cache ${args}";
            Restart = "always";
            User = "ogmios-datum-cache";
            Group = "ogmios-datum-cache";
          }
        ];
      };

    services.postgresql = {
      enable = true;
      ensureDatabases = [ cfg.postgresql.dbName ];
      ensureUsers = [
        {
          name = cfg.postgresql.user;
          ensurePermissions = {
            "DATABASE \"${cfg.postgresql.dbName}\"" = "ALL PRIVILEGES";
          };
        }
      ];
    };

    services.cardano-node = {
      enable = true;
      extraServiceConfig = _: {
        serviceConfig.TimeoutStartSec = "infinity";
        # We need to patch the cardano-node socket permissions,
        # because cardano-ogmios needs write access.
        # We also need to wait "forever" because we don't know
        # how much time cardano-node needs to process
        # the synced blocks on startup.
        serviceConfig.ExecStartPost = "${pkgs.writeShellApplication {
          name = "change-cardano-node-socket-permissions";
          text = ''
          while [ ! -S ${config.services.cardano-node.socketPath} ]; do
            sleep 1
          done
          chmod 0666 ${config.services.cardano-node.socketPath}
        '';
      }}/bin/change-cardano-node-socket-permissions";
      };
    };

    services.cardano-ogmios = {
      enable = true;
      nodeConfig = builtins.toFile "cardano-node-config.json" (builtins.toJSON config.services.cardano-node.nodeConfig);
      nodeSocket = config.services.cardano-node.socketPath;
    };
  };
}
