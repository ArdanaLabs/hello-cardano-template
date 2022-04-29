{ writeShellApplication
, entr
, projectName
}:
writeShellApplication
  {
    name = "${projectName}-feedback-loop";
    runtimeInputs = [ entr ];
    text = ''
      find docs -name "*.tex" | entr nix build .#build-docs
    '';
  }
