{ nixosTest
, cardano-ogmios
, mlabs-ogmios
, ogmios-datum-cache
, cardano-node
, ctl-server
}:
let
  name = "ogmios-datum-cache";
in
nixosTest {
  inherit name;

  nodes = {
    server = { config, pkgs, ... }: {

      imports = [
        cardano-node.nixosModules.cardano-node
        cardano-ogmios.nixosModules.ogmios
        ogmios-datum-cache
        ctl-server
      ];

      services.postgresql = {
        enable = true;
        ensureDatabases = [ name ];
        ensureUsers = [
          {
            inherit name;
            ensurePermissions = {
              "DATABASE \"${name}\"" = "ALL PRIVILEGES";
            };
          }
        ];
      };

      services.cardano-node = {
        enable = true;
        nodeConfigFile = "${cardano-node}/configuration/cardano/testnet-config.json";
        systemdSocketActivation = true;
        extraSocketConfig = i: { socketConfig.SocketMode = "0666"; }; # for some reason ogmios needs write access for the socket
      };

      services.cardano-ogmios = {
        enable = true;
        nodeConfig = "${cardano-node}/configuration/cardano/testnet-config.json";
        nodeSocket = config.services.cardano-node.socketPath;
      };

      services.ogmios-datum-cache = {
        enable = true;
        postgresql = {
          user = name;
          dbName = name;
        };
      };

      services.ctl-server.enable = true;

    };
  };
  testScript = ''
    start_all()
    server.wait_for_unit("cardano-node.service")
    server.wait_for_unit("cardano-ogmios.service")
    server.wait_for_unit("ogmios-datum-cache.service")
    server.wait_for_unit("ctl-server.service")
  '';
}
