{ nixosTest
, ctl-runtime
}:
nixosTest {
  name = "ctl-runtime config test";

  nodes = {
    server = { config, pkgs, ... }: {
      imports = [
        ctl-runtime
      ];
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
