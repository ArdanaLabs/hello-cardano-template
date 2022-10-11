{ pkgs, config, ... }:
{
  networking.firewall.allowedTCPPorts = [ 80 8080 443 ];

  security.acme = {
    acceptTerms = true;
    defaults.email = "admin@ardana.org";
    certs."ardana.org" = {
      domain = "*.ardana.org";
      listenHTTP = ":80";
    };
  };

  services.nginx = {
    enable = true;
    recommendedProxySettings = true;
    recommendedTlsSettings = true;
    virtualHosts."danaswap.ardana.org" = {
      forceSSL = true;
      enableACME = true;
      locations."/".proxyPass = "http://localhost:${toString config.services.hello-world.port}";
    };
  };

  services.hello-world = {
    enable = true;
    port = 19990;
  };
}
