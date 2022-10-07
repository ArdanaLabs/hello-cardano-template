{ nixosTest
, hello-world-server
, nixpkgs
}:
let
  name = "hello-world server";
in
nixosTest {
  inherit name;

  nodes = {
    server = hello-world-server;

    client = { pkgs, config, ... }: {
      imports = [
        "${nixpkgs}/nixos/tests/common/x11.nix"
        "${nixpkgs}/nixos/tests/common/user-account.nix"
      ];
      test-support.displayManager.auto.user = "alice";
      environment.systemPackages = [ pkgs.chromium ];
    };
  };
  testScript = ''
    start_all()
    server.wait_for_unit("cardano-node.service")
    server.wait_for_unit("cardano-ogmios.service")
    server.wait_for_unit("ogmios-datum-cache.service")
    server.wait_for_unit("ctl-server.service")
    server.wait_for_unit("hello-world-server.service")
    server.wait_for_unit("nginx")
    server.wait_for_open_port(80)
    client.wait_for_x()
    client.succeed("chromium http://danaswap.ardana.org")
    client.wait_for_window("Hello World")
    client.screenshot("hello-world")
  '';
}
