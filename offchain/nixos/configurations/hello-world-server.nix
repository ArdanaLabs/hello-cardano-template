{ cardanoNodeFlake
, cardanoOgmiosFlake
, ogmiosDatumCacheFlake
, ctlServerFlake
, helloWorldServerFlake
}:

({ pkgs, config, ... }:
{
  imports = [
    cardanoNodeFlake.nixosModules.cardano-node
    cardanoOgmiosFlake.nixosModules.ogmios
    ogmiosDatumCacheFlake.nixosModules.ogmios-datum-cache
    ctlServerFlake.nixosModules.ctl-server
    helloWorldServerFlake.nixosModules.hello-world
  ];

  networking.firewall.allowedTCPPorts = [ 80 8080 443 ];

  security.acme = {
    acceptTerms = true;
    defaults.email = "admin@ardana.org";
    certs."ardana.org" = {
      domain = "*.ardana.org";
      listenHTTP = ":80";
    };
  };

  services.nginx = {
    enable = true;
    recommendedProxySettings = true;
    recommendedTlsSettings = true;
    virtualHosts."danaswap.ardana.org" = {
      forceSSL = true;
      enableACME = true;
      locations."/".proxyPass = "http://localhost:${toString config.services.hello-world.port}";
    };
  };

  services.cardano-node = {
    enable = true;
    environment = "testnet";
    nodeConfigFile = "${cardanoNodeFlake}/configuration/cardano/${config.services.cardano-node.environment}-config.json";
    topology = "${cardanoNodeFlake}/configuration/cardano/${config.services.cardano-node.environment}-topology.json";
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
    nodeConfig = "${cardanoNodeFlake}/configuration/cardano/${config.services.cardano-node.environment}-config.json";
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

  services.hello-world = {
    enable = true;
    port = 19990;
  };
})
