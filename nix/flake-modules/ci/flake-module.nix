{ ... }: {
  flake = {
    # we only have an agent for these systems
    herculesCI.ciSystems = [ "x86_64-linux" ];
  };
}
