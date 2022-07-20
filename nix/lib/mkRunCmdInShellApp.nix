{ writeShellApplication
, jq
, coreutils
, ...
}:
{ scriptName
, devshellName
, command
,
}: {
  type = "app";
  program = writeShellApplication {
    name = scriptName;
    runtimeInputs = [ jq coreutils ];
    text = ''
      PROJ_DIR="$(nix flake metadata --json | jq .locked.url -cr | cut --characters=8-)"
      cd "$PROJ_DIR"
      nix develop .#${devshellName} -c sh -c "${command}"
    '';
  };
}
