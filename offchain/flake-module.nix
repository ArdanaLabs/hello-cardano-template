{ self, ... }:
{
  perSystem = system: { config, self', inputs', ... }:
    let
      # A flake-module in nix/flake-modules/haskell.nix defines haskell-nix
      # packages once, so we can reuse it here, it's more performant.
      pkgs = config.haskell-nix.pkgs;
      # dusd-lib contains helper functions for dealing with haskell.nix. From it,
      # we inherit plutusProjectIn
      dusd-lib = import "${self}/nix/lib/haskell.nix" { inherit system self; };
      inherit (dusd-lib) plutusProjectIn;

      onchain-scripts = self'.packages.onchain-scripts;
      subdir = "offchain";
      # Checks the shell script using ShellCheck
      checkedShellScript = system: name: text:
        (pkgs.writeShellApplication {
          inherit name text;
        }) + "/bin/${name}";
      haskellNixFlake = project.flake { };
      project = plutusProjectIn {
        inherit subdir;
        inherit pkgs;
        extraPackages = {
          hello-world.components.library.preBuild = "export DUSD_SCRIPTS=${onchain-scripts}";
        };
        extraShell = {
          additional = ps: [
            ps.plutus-contract
            ps.plutus-ledger
            ps.plutus-ledger-api
            ps.plutus-ledger-constraints
            ps.plutus-pab
            ps.plutus-use-cases
          ];
          DUSD_SCRIPTS = onchain-scripts;
          propagatedBuildInputs = [
            # cardano-node and cardano-cli need to be on the PATH to run the
            # cluster + PAB.
            self.inputs.cardano-node.outputs.packages.${system}."cardano-node:exe:cardano-node"
            self.inputs.cardano-node.outputs.packages.${system}."cardano-cli:exe:cardano-cli"
          ];
          tools = {
            ghcid = { };
            haskell-language-server = { };
          };
        };
        pkg-def-extras = [
          (hackage: {
            packages = {
              cryptostore = (((hackage."cryptostore")."0.2.1.0").revisions).default;
              jwt = (((hackage."jwt")."0.11.0").revisions).default;
              random = (((hackage.random)."1.2.1").revisions).default;
            };
          })
        ];
        sha256map = import ./sha256map;
      };
    in
    {
      devShells.offchain = haskellNixFlake.devShell.overrideAttrs (oa: {
        shellHook = oa.shellHook + ''
          # running local cluster + PAB
          export SHELLEY_TEST_DATA="${self.inputs.plutus-apps}/plutus-pab/local-cluster/cluster-data/cardano-node-shelley/"
        '';
      });
      packages = haskellNixFlake.packages // { };
      apps = {
        offchain-test = {
          type = "app";
          program = checkedShellScript system "dUSD-offchain-test"
            '' export DUSD_SCRIPTS=${onchain-scripts}
                cd ${self}
                ${haskellNixFlake.packages."dUSD-offchain:exe:tests"}/bin/tests;
            '';
        };
      };
    };
  flake = {
  };
}


