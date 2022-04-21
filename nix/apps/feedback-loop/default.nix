{ writeShellApplication
, entr
}:
writeShellApplication
  {
    name = "feedback-loop";
    runtimeInputs = [ entr ];
    text = ''
      find docs -name "*.tex" | entr nix build .#build-docs
    '';
  }
