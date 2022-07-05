{
  nixosTest
, sslCertificate
, sslCertificateKey
, priceFeederExe
, binanceMockServerExe
, coinbaseMockServerExe
, huobiMockServerExe
, krakenMockServerExe
, kucoinMockServerExe
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
  createServerMockService = { serverMockExe
                            , user
                            , name
                            , port
                            , priceDataJSONFile
                            }: {
                              wantedBy = [ "multi-user.target" ]; 
                              after = [ "network.target" ];
                              description = "${name}-server-mock";
                              serviceConfig = {
                                User = user;
                                Group = user;
                                StateDirectory = dirOf priceDataJSONFile;
                                ExecStart = ''${serverMockExe}/bin/${name}-mock-servant-server ${priceDataJSONFile} ${toString port}'';
                              };
                            };
in nixosTest {
  name = "price-feeder-integration";

  nodes = {
    server = { config, pkgs, ... }: {
      virtualisation.vlans = [ 1 ];
      systemd.tmpfiles.rules = [
        "D '${workingDir}' 755 ${testUser} ${testUser} - -"
      ];

      users.users.test = {
        name = testUser;
        group = testUser;
        isSystemUser = true;
        home = workingDir;
        useDefaultShell = true;
      };
      users.groups.test = {};

      networking.interfaces.eth1.ipv4.addresses = [
        { address = serverIpAddress; prefixLength = 24; }
      ];

      systemd.services.kraken-mock-server = createServerMockService {
                                              user = testUser;
                                              serverMockExe = krakenMockServerExe;
                                              name = "kraken";
                                              port = krakenPort;
                                              inherit priceDataJSONFile;
                                            };

      networking.firewall.allowedTCPPorts = [ 443 ];
      security.pki.certificateFiles = [ sslCertificate ];
      services.nginx = {
        enable = true;
        recommendedProxySettings = true;
        virtualHosts."api.kraken.com" = {
          forceSSL = true;
          enableACME = false;
          inherit sslCertificate;
          inherit sslCertificateKey;
          locations."/0/".proxyPass = "http://127.0.0.1:${toString krakenPort}/";
        };
      };
    };
    client = { config, pkgs, ... }: {
      virtualisation.vlans = [ 1 ];
      security.pki.certificateFiles = [ sslCertificate ];
      environment.systemPackages = [ pkgs.curl pkgs.iputils priceFeederExe ];
      networking.hosts = {
        "${serverIpAddress}" = [ "api.kraken.com" ];
        "192.0.2.0" = [ "api.huobi.pro" "api.kucoin.com" "api.coinbase.com" "api.binance.com" ];
      };
    };
  };
  testScript = ''
    import json
    price = 123.4
    priceData = { "_krakenPrice": price }

    start_all()
    server.wait_for_unit("nginx.service")
    server.succeed("echo '{}' > ${priceDataJSONFile}".format(json.dumps(priceData)))
    (_, response) = server.execute("curl -k http://localhost:${toString krakenPort}/public/Ticker?pair=ADAUSD")
    assert (str(price) in response), "Couldn't find the price {} in the server curl response {}".format(str(price), response)

    client.wait_for_unit("default.target")
    client.wait_until_succeeds("ping -n -c 1 api.kraken.com")

    (_, response) = client.execute("curl -k https://api.kraken.com/0/public/Ticker?pair=ADAUSD")
    assert (str(price) in response), "Couldn't find the price {} in the client curl response {}".format(str(price), response)

    (_, output) = client.execute("ada-price-feeder -N 1 --disable-cert-validation 2>&1")
    assert (str(price) in output), "Couldn't find the price {} in the price-feeder output {}".format(str(price), output)
  '';
}
