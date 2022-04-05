{ writeShellApplication
, entr
}:
writeShellApplication
  {
    name = "feedback-loop";
    runtimeInputs = [ entr ];
    text = ''
      ls docs/*.tex | entr nix build .#build-docs
    '';
  }
