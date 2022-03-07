# Feedback loops

You can set up a feedback loop through
```
echo "test-plan.tex" | entr latexmk -pdf test-plan.tex
```
within a nix shell (`nix develop`), or by running
```
nix run .#feedback-loop
```



