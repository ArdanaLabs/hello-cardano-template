{ self, lib, ... }:
{
  perSystem = { config, self', inputs', system, ... }:
    let
      pkgs = inputs'.nixpkgs.legacyPackages;
      purs-nix = config.ps.purs-nix;
      all-ps-pkgs = config.ps.pkgs;
      inherit (config) cat-lib offchain-lib;

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
        network = {
          name = "preview";
          magic = 2;
        };
        datumCache.blockFetcher.firstBlock = {
          slot = 3162738;
          id = "fed1bf7331a8f5dade0a6087b31218bb4552bdb722db450e4ecf66740395a93b";
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
      makeTestAllApp = mode:
        let
          getTestScript = outputName:
            self'.apps."offchain:${outputName}".program;
          runTests =
            pkgs.writeScript "run-tests" ''
              # --will-cite gets rid of the annoying citation notice
              # we disable the shellcheck that says things wont expand in singlequote
              # because it's a false positive
              # shellcheck disable=SC2016
              ${pkgs.parallel}/bin/parallel --will-cite \
                '. {} &> "$(basename {.})-output"' ::: \
                ${getTestScript "hello-world-api:test:${mode}"} \
                ${getTestScript "hello-world-cli:test:${mode}"} \
                ${getTestScript "hello-world-browser:test:${mode}"}
              printf "$?" > "$TEST_EXITCODE_FILE"
            '';
        in
        cat-lib.mkApp (
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

                function print_logs {
                  for file in "''${outfiles[@]}"; do
                    echo -e "$file: $(grep . "$file" | tail -qn 1)"
                  done
                }

                # remove output log files
                rm -f ./*-output
                # print if parallel is still running
                while true; do
                  outfiles=(*-output)
                  print_logs
                  # sleep 1 second between updates
                  sleep 1
                  # clear the amount of lines we printed
                  tput cuu ''${#outfiles[@]}
                  tput ed
                  # check if the test command still running or not
                  if ! ps -p $TEST_PID > /dev/null; then
                    print_logs
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
    in
    {
      apps = {
        ctl-runtime = ctl-pkgs.launchCtlRuntime ctlRuntimeConfig;
        "offchain:docs:serve" =
          cat-lib.makeServeApp
            "${self'.packages."offchain:docs"}/html/";
        "offchain:test:local" = makeTestAllApp "local";
        "offchain:test:testnet" = makeTestAllApp "testnet";
      };
      packages = {
        "offchain:hello-world-cbor" = hello-world-cbor;
        "offchain:docs" =
          pkgs.runCommand "offchain-all-docs" { }
            ''
              mkdir -p $out
              # link hello-world-api docs
              ln -sf ${self'.packages."offchain:hello-world-api:docs"}/generated-docs/html $out/html
              if ! [ -f "$out/html/index.html" ]; then
                echo "doc generation did not create index.html in the expected location"
                exit 1
              fi
            '';
      };
    };
}
