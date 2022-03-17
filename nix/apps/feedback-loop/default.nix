{ writeShellApplication
, entr
, texlive
}:
let
  latexEnv = texlive.combine { inherit ( texlive ) scheme-basic latexmk; };
in
writeShellApplication
  {
    name = "feedback-loop";
    runtimeInputs = [ entr latexEnv ];
    text =
      ''
      echo "docs/test-plan.tex" | entr latexmk -output-directory="./docs/test-plan/" -pdf docs/test-plan.tex
      '';
  }
