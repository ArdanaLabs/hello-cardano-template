{
  nixosTest
, krakenMockServerExe
}:
let
  binancePort = 5000;
  coinbasePort = 5001;
  huobiPort = 5002;
  krakenPort = 5003;
  kucoinPort = 5004;
  priceDataJSONFile = "/tmp/test/pricedata.json";
  mkMockService = { mockServerExe
                  , name
                  , port
                  , priceDataJSONFile
                  }: {
                    wantedBy = [ "multi-user.target" ]; 
                    after = [ "network.target" ];
                    description = "Starts the ${name}-server-mock";
                    serviceConfig = {
                      User = name;
                      Group = name;
                      StateDirectory = dirOf priceDataJSONFile;
                      ExecStart = ''${krakenMockServerExe}/bin/${name}-server-mock ${priceDataJSONFile} ${toString port}'';
                    };
                  };
in nixosTest {
  name = "price-feeder-integration";

  nodes = {
    server = { config, pkgs, ... }: {
      systemd.services.kraken-mock-server = mkMockService {
                                              mockServerExe = krakenMockServerExe;
                                              name = "kraken";
                                              port = krakenPort;
                                              inherit priceDataJSONFile;
                                            };
      networking.firewall.allowedTCPPorts = [ krakenPort ];
    };
    client = {
    };
  };
  testScript = ''
    import json
    import sys

    start_all()

    server.wait_for_open_port(${toString krakenPort})
  '';
}