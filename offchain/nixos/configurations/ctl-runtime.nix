{ pkgs, config, ... }:
{

  services.cardano-node = {
    enable = true;
    environment = "testnet";
    extraServiceConfig = _: {
      serviceConfig.TimeoutStartSec = "infinity";
      serviceConfig.ExecStartPost = pkgs.writeShellScript "change-cardano-node-socket-permissions" ''
        while [ ! -S ${config.services.cardano-node.socketPath} ]; do
          sleep 1
        done
        chmod 0666 ${config.services.cardano-node.socketPath}
      '';
    };
  };

  services.cardano-ogmios = {
    enable = true;
    nodeConfig = builtins.toFile "cardano-node-config.json" (builtins.toJSON config.services.cardano-node.nodeConfig);
    nodeSocket = config.services.cardano-node.socketPath;
  };

  services.ogmios-datum-cache = {
    enable = true;
    postgresql = {
      user = "ogmios-datum-cache";
      dbName = "ogmios-datum-cache";
    };
  };

  services.postgresql = {
    enable = true;
    ensureDatabases = [ config.services.ogmios-datum-cache.postgresql.dbName ];
    ensureUsers = [
      {
        name = config.services.ogmios-datum-cache.postgresql.user;
        ensurePermissions = {
          "DATABASE \"${config.services.ogmios-datum-cache.postgresql.dbName}\"" = "ALL PRIVILEGES";
        };
      }
    ];
  };

  services.ctl-server.enable = true;

}
