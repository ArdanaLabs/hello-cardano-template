{ writeShellApplication
, writeScript
, jq
, coreutils
, ...
}:
{ scriptName ? throw "please specify a script name"
, directory ? "."
, devshellName ? throw "please specify devshellName"
, devshellCommand ? throw "please specify devshellCommand"
, command ? "nix develop .#${devshellName} -c sh -c \"${devshellCommand}\""
,
}: {
  type = "app";
  program =
    writeShellApplication
      {
        name = scriptName;
        runtimeInputs = [ jq coreutils ];
        text = ''
          PROJ_DIR="$(nix flake metadata --json | jq .locked.url -cr | cut --characters=8-)"
          cd "$PROJ_DIR"/${directory}
          ${command}
        '';
      };
}
