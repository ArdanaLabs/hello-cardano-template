{ self, ... }:
{
  perSystem = system: { config, self', inputs', ... }:
    let
      pkgs = import self.inputs.nixpkgs {
                  inherit system;
                  overlays = [ self.inputs.cardano-transaction-lib.overlay.${system} ];
                };
      offchainPsProject = pkgs.purescriptProject {
        inherit pkgs;
        projectName = "offchain-ctl";
        src = ./.;
      };
      hello-world-api = offchainPsProject.bundlePursProject {
        sources = [ "hello-world-api/src" ];
        main = "Main";
      };
      config = {
        datumCache.blockFetcher.firstBlock = {
          slot = 60854917;
          id = "1c157a01120772c35d468b425e6ef228d5b6cec5977f7897540aa8d0870f9ab9";
        };
      };
    in
    {
      packages = {
        inherit hello-world-api;
        ctl-scaffold-runtime = pkgs.buildCtlRuntime { };
      };
      apps = {
        ctl-scaffold-runtime = pkgs.launchCtlRuntime config;
      };
      devShells.offchain-ctl = offchainPsProject.devShell;
    };
  flake = {
  };
}