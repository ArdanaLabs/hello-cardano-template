{ lib, pkgs, config, ... }:
let
  inherit (lib)
    types
    mkOption
    mkIf
    mkMerge
    mkEnableOption
    mdDoc
    ;
  cfg = config.services.hello-world;
in
{
  options.services.hello-world = {

    enable = mkEnableOption "enable serving the hello-world application";

    package = mkOption {
      type = types.package;
    };

    port = mkOption {
      type = types.port;
      default = 55555;
      example = 55555;
      description = mdDoc ''
        Port to listen on.
      '';
    };

    ctlRuntimeConfig = {
      ogmiosConfig = {
        host = mkOption {
          type = types.str;
          default = "127.0.0.0";
          example = "ogmios-service.com";
          description = mdDoc ''
            Ogmios service host address.
          '';
        };
        port = mkOption {
          type = types.port;
          default = 1337;
          example = 1337;
          description = mdDoc ''
            Ogmios service host port.
          '';
        };
      };
      datumCacheConfig = {
        host = mkOption {
          type = types.str;
          default = "127.0.0.0";
          example = "odt-service.com";
          description = mdDoc ''
            Ogmios datum cache service host address.
          '';
        };
        port = mkOption {
          type = types.port;
          default = 9999;
          example = 9999;
          description = mdDoc ''
            Ogmios datum cache service host port.
          '';
        };
      };
      ctlServerConfig = {
        host = mkOption {
          type = types.str;
          default = "127.0.0.0";
          example = "ctl-server.com";
          description = mdDoc ''
            CTL server host address.
          '';
        };
        port = mkOption {
          type = types.port;
          default = 8081;
          example = 8081;
          description = mdDoc ''
            CTL server host port.
          '';
        };
      };
    };

  };

  config = mkIf cfg.enable {

    systemd.services.hello-world =
      let
        packageWithCtlRuntimeConfig = pkgs.runCommand "package-with-ctl-runtime-config" { } ''
          mkdir -p $out/dist
          echo '${builtins.toJSON cfg.ctlRuntimeConfig}' > $out/dist/ctl-runtime-config.json
          cp -r ${cfg.package}/* $out/
          ls -lisa $out/dist
        '';
      in
      {
        description = "hello-world";
        documentation = [ "https://github.com/ArdanaLabs/cardano-app-template" ];
        wantedBy = [ "multi-user.target" ];
        after = [ "ctl-server.service" "ogmios-datum-cache.service" "networking.target" ];
        serviceConfig = mkMerge [
          {
            ExecStart = ''${pkgs.simple-http-server}/bin/simple-http-server -c=js,css,svg,html -i -p ${toString cfg.port} -- ${packageWithCtlRuntimeConfig}'';
            Restart = "always";
            DynamicUser = true;
          }
        ];
      };

    services.ctl-runtime.enable = true;
  };
}
