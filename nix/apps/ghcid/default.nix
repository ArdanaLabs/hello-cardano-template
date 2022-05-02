{ writeShellApplication
, ghcid
, projectName
, cabalProjectRoot
, name
, args
}:
writeShellApplication 
  { 
    name = "${projectName}-${name}"; 
    text = ''
      cd "${cabalProjectRoot}" && ${ghcid}/bin/ghcid ${args}
    '';
  }
