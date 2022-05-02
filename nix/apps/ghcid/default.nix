{ writeShellApplication
, ghcid
, projectName
, cabalProjectRoot
, name
, args
}:
writeShellApplication 
  { 
    name = "${projectName}-ghcid-${name}"; 
    text = ''
      cd "${cabalProjectRoot}" && ${ghcid}/bin/ghcid ${args}
    '';
  }
