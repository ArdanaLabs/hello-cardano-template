{
  nixosTest
, cardano-ogmios
, ogmios-datum-cache
}:
let
  serverIpAddress = "192.168.1.199";
  binancePort = 5000;
  coinbasePort = 5001;
  huobiPort = 5002;
  krakenPort = 5003;
  kucoinPort = 5004;
  testUser = "test";
  workingDir = "/tmp/test";
  priceDataJSONFile = "${workingDir}/pricedata.json";

in nixosTest {
  name = "ogmios-datum-cache";

  nodes = {
    server = { config, pkgs, ... }: {

      imports = [
        cardano-ogmios.nixosModules.ogmios
        ogmios-datum-cache
      ];

      virtualisation.vlans = [ 1 ];
      systemd.tmpfiles.rules = [
        "D '${workingDir}' 755 ${testUser} ${testUser} - -"
      ];

      services.postgresql = {
        enable = true;
      };

      services.cardano-ogmios = {
        enable = true;
      };

      services.ogmios-datum-cache = {
        enable = true;
      };

    };
  };
  testScript = ''
    start_all()
    server.wait_for_unit("ogmios-datum-cache.service")
  '';
}