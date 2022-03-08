# dUSD
The Ardana Dollar Stablecoin

## Architecture docs
See `./architecture`

## Technologies
On chain code will be developed in Plutarch.

## Testing
Testing of on chain code will be done with apropos logical specifications.

## Latex test plan documentation

You can set up a feedback loop through
```
echo "test-plan.tex" | entr latexmk -pdf test-plan.tex
```
within a nix shell (`nix develop`), or by running
```
nix run .#feedback-loop
```



