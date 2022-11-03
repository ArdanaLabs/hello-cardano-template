{ nixosTest
, hello-world
, nixpkgs
}:
let
  helloWorldServerPort = 8080;
  log = a: builtins.trace (builtins.attrNames a) a;
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
        ctlRuntimeConfig.local = {
          ctlServerConfig = {
            host = "127.0.0.1";
            port = 8081;
            secure = false;
          };
          ogmiosConfig = {
            host = "127.0.0.1";
            port = 8082;
            secure = false;
          };
          datumCacheConfig = {
            host = "127.0.0.1";
            port = 8083;
            secure = false;
          };
        };
        ctlRuntimeConfig.public = {
          ctlServerConfig = {
            host = "server";
            port = 8081;
            secure = false;
          };
          ogmiosConfig = {
            host = "server";
            port = 8082;
            secure = false;
          };
          datumCacheConfig = {
            host = "server";
            port = 8083;
            secure = false;
          };
        };
      };
      networking.firewall.allowedTCPPorts = [
        helloWorldServerPort
        config.services.hello-world.ctlRuntimeConfig.public.ctlServerConfig.port
        config.services.hello-world.ctlRuntimeConfig.public.ogmiosConfig.port
        config.services.hello-world.ctlRuntimeConfig.public.datumCacheConfig.port
      ];
    };

    client = { pkgs, config, ... }: {
      imports = [
        "${nixpkgs}/nixos/tests/common/x11.nix"
        "${nixpkgs}/nixos/tests/common/user-account.nix"
      ];
      virtualisation.memorySize = 2048;
      test-support.displayManager.auto.user = "alice";
      environment = {
        systemPackages = with pkgs; [ chromium xdotool curl ];
        variables."XAUTHORITY" = "/home/alice/.Xauthority";
      };
    };
  };

  enableOCR = true; # required by wait_for_text, OCR = optical character recognition

  testScript = ''
    import json
    import time

    start_all()
    server.wait_for_unit("cardano-node.service")
    server.wait_for_unit("cardano-ogmios.service")
    server.wait_for_unit("ogmios-datum-cache.service")
    server.wait_for_unit("ctl-server.service")
    server.wait_for_unit("hello-world.service")
    server.wait_for_open_port(${builtins.toString helloWorldServerPort})

    client.wait_for_x()

    with subtest("check ctl-runtime-config.json"):
      ctl_runtime_config = json.loads(client.succeed("curl http://server:${builtins.toString helloWorldServerPort}/dist/ctl-runtime-config.json"))
      assert ctl_runtime_config["ctlServerConfig"] ["host"] == "server"
      assert ctl_runtime_config["ctlServerConfig"] ["port"] == 8081
      assert ctl_runtime_config["ogmiosConfig"] ["host"] == "server"
      assert ctl_runtime_config["ogmiosConfig"] ["port"] == 8082
      assert ctl_runtime_config["datumCacheConfig"] ["host"] == "server"
      assert ctl_runtime_config["datumCacheConfig"] ["port"] == 8083
    
    client.succeed("su - alice -c 'ulimit -c unlimited; chromium http://server:${builtins.toString helloWorldServerPort} >&2 & disown'")
    client.wait_for_window("Hello World")
    # focus the chromium window displaying Hello World
    client.succeed("xdotool search --sync --onlyvisible --name 'Hello World' windowfocus --sync windowactivate --sync")
    time.sleep(2)
    client.screenshot("hello_world")
  '';
}
