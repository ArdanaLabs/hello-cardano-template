{ writeShellApplication
, entr
}:
writeShellApplication
  {
    name = "feedback-loop";
    runtimeInputs = [ entr ];
    text = ''
      echo "docs/test-plan.tex" | entr nix build .#test-plan
    '';
  }
