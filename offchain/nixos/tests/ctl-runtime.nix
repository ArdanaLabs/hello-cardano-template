{ nixosTest
, ctl-runtime
}:
nixosTest {
  name = "ctl-runtime test";

  nodes = {
    server = { config, pkgs, ... }: {
      imports = [
        ctl-runtime
      ];
      services.ctl-runtime.enable = true;
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
