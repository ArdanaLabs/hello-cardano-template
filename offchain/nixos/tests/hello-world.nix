{ nixosTest
, hello-world
, nixpkgs
}:
let
  helloWorldServerPort = 8080;
in
nixosTest {
  name = "hello-world-test";

  nodes = {
    server = { pkgs, config, ... }: {
      imports = [
        hello-world
      ];
      services.hello-world = {
        enable = true;
        port = helloWorldServerPort;
      };
      networking.firewall.allowedTCPPorts = [ helloWorldServerPort ];
    };

    client = { pkgs, config, ... }: {
      imports = [
        "${nixpkgs}/nixos/tests/common/x11.nix"
        "${nixpkgs}/nixos/tests/common/user-account.nix"
      ];
      virtualisation.memorySize = 2048;
      test-support.displayManager.auto.user = "alice";
      environment = {
        systemPackages = [ pkgs.chromium pkgs.xdotool ];
        variables."XAUTHORITY" = "/home/alice/.Xauthority";
      };
    };
  };

  enableOCR = true; # required by wait_for_text, OCR = optical character recognition

  testScript = ''
    import time

    start_all()
    server.wait_for_unit("cardano-node.service")
    server.wait_for_unit("cardano-ogmios.service")
    server.wait_for_unit("ogmios-datum-cache.service")
    server.wait_for_unit("ctl-server.service")
    server.wait_for_unit("hello-world.service")
    server.wait_for_open_port(${builtins.toString helloWorldServerPort})

    client.wait_for_x()
    client.succeed("su - alice -c 'ulimit -c unlimited; chromium http://server:${builtins.toString helloWorldServerPort} >&2 & disown'")
    client.wait_for_window("Hello World")
    # focus the chromium window displaying Hello World
    client.succeed("xdotool search --sync --onlyvisible --name 'Hello World' windowfocus --sync windowactivate --sync")
    time.sleep(2)
    client.screenshot("hello_world")
  '';
}
