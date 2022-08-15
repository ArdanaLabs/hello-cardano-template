{ self, lib, ... }:
{
  perSystem = { config, self', inputs', system, ... }:
    let
      pkgs = inputs'.nixpkgs.legacyPackages;
      purs-nix = config.ps.purs-nix;
      all-ps-pkgs = config.ps.pkgs;
      inherit (config) dusd-lib offchain-lib;

      # Ideally we would just append the CTL overlay to the haskell-nix pkgs
      # we already have at `config.haskell-nix.pkgs`, but our haskell-nix
      # instances seem to be incompatible. So we just use CTLs haskell-nix here.
      ctl-pkgs = import self.inputs.nixpkgs {
        inherit system;
        overlays = with self.inputs.cardano-transaction-lib; [
          inputs.haskell-nix.overlay
          inputs.iohk-nix.overlays.crypto
          overlays.runtime
        ];
      };

      # use more recent slot to avoid long sync time
      ctlRuntimeConfig = {
        datumCache.blockFetcher.firstBlock = {
          slot = 62153233;
          id = "631c621b7372445acf82110282ba72f4b52dafa09c53864ddc2e58be24955b2a";
        };
      };

      hello-world-cbor =
        purs-nix.build
          {
            name = "hello-world-cbor";
            src.path = self'.packages."onchain:hello-world-cbor-purs";
            info.dependencies = [ ];
            info.version = "0.0.1";
          };
    in
    {
      apps = {
        ctl-runtime = ctl-pkgs.launchCtlRuntime ctlRuntimeConfig;
        "offchain:docs:serve" =
          dusd-lib.makeServeApp
            "${self'.packages."offchain:docs"}/generated-docs/html/";
        "offchain:test" =
          let
            getTestScript = outputName:
              self'.apps."offchain:${outputName}:test".program;
            runTests =
              pkgs.writeScript "run-tests" ''
                # --will-cite gets rid of the annoying citation notice
                # we disable the shellcheck that says things wont expand in singlequote
                # because it's a false positive
                # shellcheck disable=SC2016
                ${pkgs.parallel}/bin/parallel --will-cite \
                  '. {} &> "$(basename {.})-output"' ::: \
                  ${getTestScript "hello-world-api"} \
                  ${getTestScript "hello-world-browser"} \
                  ${getTestScript "hello-world-cli"}
                printf "$?" > "$TEST_EXITCODE_FILE"
              '';
          in
          dusd-lib.mkApp (
            pkgs.writeShellApplication
              {
                name = "offchain-test-all";
                runtimeInputs = with pkgs; [ coreutils psutils ncurses ];
                text = ''
                  shopt -s nullglob
                  # create a file to store parallel exit code in
                  TEST_EXITCODE_FILE="$(mktemp)"

                  # run tests in background
                  export TEST_EXITCODE_FILE
                  ${runTests} &

                  # get our parallel command PID
                  TEST_PID=$!
                  # set a trap so if we CTRL+C the script test command is killed
                  trap 'kill $TEST_PID' EXIT

                  # print if parallel is still running
                  while true; do
                    outfiles=(*-output)
                    # print logs
                    for file in "''${outfiles[@]}"; do
                      echo -e "$file: $(grep . "$file" | tail -qn 1)"
                    done
                    # sleep 1 second between updates
                    sleep 1
                    # check if the test command still running or not
                    if ps -p $TEST_PID > /dev/null; then
                      # clear the amount of lines we printed
                      tput cuu ''${#outfiles[@]}
                      tput ed
                    else
                      break
                    fi
                  done

                  # remove trap since here the test command will have exit already
                  trap - EXIT
                  # exit with the exitcode of our test command
                  exit "$(cat "$TEST_EXITCODE_FILE")"
                '';
              }
          );
      };
      packages = {
        "offchain:hello-world-cbor" = hello-world-cbor;
        "offchain:docs" =
          pkgs.runCommand "offchain-all-docs" { }
            ''
              mkdir -p $out
              # link hello-world-api docs
              ln -sf ${self'.packages."offchain:hello-world-api:docs"} $out/hello-world-api
            '';
      };
    };
}
