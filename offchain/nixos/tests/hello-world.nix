{ nixosTest
, hello-world-server
, nixpkgs
}:
nixosTest {
  name = "hello-world-server-configuration-test";

  nodes = {
    server = { pkgs, config, ... }: {
      imports = [
        hello-world-server
      ];
      services.nginx = {
        # disable TLS for the test
        recommendedProxySettings = pkgs.lib.mkForce false;
        recommendedTlsSettings = pkgs.lib.mkForce false;
      };
    };

    client = { pkgs, config, ... }: {
      imports = [
        "${nixpkgs}/nixos/tests/common/x11.nix"
        "${nixpkgs}/nixos/tests/common/user-account.nix"
      ];
      virtualisation.memorySize = 2048;
      test-support.displayManager.auto.user = "alice";
      environment = {
        systemPackages = [ pkgs.chromium ];
        variables."XAUTHORITY" = "/home/alice/.Xauthority";
      };
    };
  };

  testScript = ''
    start_all()
    server.wait_for_unit("cardano-node.service")
    server.wait_for_unit("cardano-ogmios.service")
    server.wait_for_unit("ogmios-datum-cache.service")
    server.wait_for_unit("ctl-server.service")
    server.wait_for_unit("hello-world.service")
    server.wait_for_unit("nginx")
    server.wait_for_open_port(80)

    client.wait_for_x()
    client.succeed("su - alice -c 'ulimit -c unlimited; chromium http://server:80 >&2 & disown'")
    client.wait_for_window("Hello World")
    client.screenshot("hello_world")
  '';
}
