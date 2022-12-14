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

    enable = mkEnableOption (mdDoc ''
      hello-world application.
      This will enable the ctl-runtime services
    '');

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

    environment = mkOption {
      type = types.str;
      default = "mainnet";
      example = "testnet";
      description = mdDoc ''
        The cardano environment we want to deploy to.
        This option will set the environment parameter
        for cardano-node.
      '';
    };

    ctlRuntimeConfig =
      let
        serverConfig = {
          host = mkOption {
            type = types.str;
            example = "service.example.com";
            description = mdDoc ''
              Service host address.
            '';
          };
          port = mkOption {
            type = types.port;
            description = mdDoc ''
              Service port.
            '';
          };
          secure = mkOption {
            type = types.bool;
            default = true;
            description = mdDoc ''
              Whether to use https or not.
            '';
          };
          path = mkOption {
            type = types.str;
            default = "";
            description = mdDoc ''
              Path segment of service URL address.
            '';
          };
        };
        ctlRuntimeOptions = {
          ogmiosConfig = serverConfig;
          datumCacheConfig = serverConfig;
          ctlServerConfig = serverConfig;
        };

      in
      {
        public = mkOption {
          type = types.nullOr (types.submodule { options = ctlRuntimeOptions; });
          default = null;
          description = mdDoc ''
            The public options are used to configure the
            hello-world client code, such that it uses these public URLs
            when making requests. You want to set these options e.g. 
            when the CTL runtime is deployed behind a reverse proxy.
            The analogous local options can then then be used
            to configure the reverse proxy to point to the actual local
            location.
          '';
        };
        local = mkOption {
          type = types.submodule { options = ctlRuntimeOptions; };
        };
      };

  };

  config = mkIf cfg.enable {

    systemd.services.hello-world =
      let
        packageWithCtlRuntimeConfig =
          let
            ctlRuntimeConfig =
              if builtins.isNull cfg.ctlRuntimeConfig.public
              then
              # if we pass an empty string to the ConfigParams of CTLs client-code it will
              # automatically append a forward slash `/` and we don't want that,
              # as CTL does not normalize URLs or paths right now.
                builtins.filterAttrs
                  (attrName: attrValue: attrName == "path" && attrValue == "")
                  cfg.ctlRuntimeConfig.local
              else cfg.ctlRuntimeConfig.public;
          in
          pkgs.runCommand "package-with-ctl-runtime-config" { } ''
            mkdir -p $out/dist
            echo '${builtins.toJSON ctlRuntimeConfig}' > $out/dist/ctl-runtime-config.json
            cp -r ${cfg.package}/* $out/
          '';
      in
      {
        description = "hello-world";
        documentation = [ "https://github.com/ArdanaLabs/hello-cardano-template" ];
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

    services.cardano-node.environment = cfg.environment;

    services.ctl-runtime.enable = true;

    services.cardano-ogmios = {
      port = cfg.ctlRuntimeConfig.local.ogmiosConfig.port;
      hostAddr = cfg.ctlRuntimeConfig.local.ogmiosConfig.host;
    };
    services.ogmios-datum-cache.port = cfg.ctlRuntimeConfig.local.datumCacheConfig.port;
    services.ctl-server.port = cfg.ctlRuntimeConfig.local.ctlServerConfig.port;
  };
}
