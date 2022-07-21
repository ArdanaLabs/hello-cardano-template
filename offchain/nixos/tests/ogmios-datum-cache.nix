{
  nixosTest
, cardano-ogmios
, ogmios-datum-cache
, cardano-node
}:
let
  psDbName = "ctxlib";
  psDbUser = "ogmios-datum-cache";
in nixosTest {
  name = "ogmios-datum-cache";

  nodes = {
    server = { config, pkgs, ... }: {

      imports = [
        cardano-ogmios.nixosModules.ogmios
        cardano-node.nixosModules.cardano-node
        ogmios-datum-cache
      ];

      virtualisation.vlans = [ 1 ];
      systemd.tmpfiles.rules = [
        "D '${workingDir}' 755 ${testUser} ${testUser} - -"
      ];

      services.postgresql = {
        enable = true;
        ensureDatabases = [ psDbName ];
        ensureUsers = [ 
          {
            name = psDbUser;
            ensurePermissions = {
              "DATABASE \"${psDbName}\"" = "ALL PRIVILEGES";
            };
          }
        ];
      };

      services.cardano-node = {
        enable = true;
        nodeConfigFile = "${cardano-node}/configuration/cardano/testnet-config.json";
      };

      services.cardano-ogmios = {
        enable = true;
        nodeConfig = "${cardano-node}/configuration/cardano/testnet-config.json"; 
        nodeSocket = config.services.cardano-node.socketPath;
      };

      services.ogmios-datum-cache = {
        enable = true;
        postgresql = {
          user = psDbUser;
        };
      };

    };
  };
  testScript = ''
    start_all()
    server.wait_for_unit("ogmios-datum-cache.service")
  '';
}