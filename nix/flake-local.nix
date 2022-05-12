# TODO: Remove this in favour of https://github.com/ArdanaLabs/dUSD/issues/120
{ inputs, system }:

{
  # In flake apps, there is no builtin way to access the project root, where
  # flake.nix lives. To workaround this, we inject it as env var in the
  # `shellHook`. This is abstracted away as `flakeLocal`.
  flakeLocal = {
    shellHook = ''
      export FLAKE_ROOT=$(pwd)
    '';
    absPath = relPath: "$FLAKE_ROOT/${relPath}";
  };

}
