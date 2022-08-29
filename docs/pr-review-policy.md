# PR Review Guidelines
This is a living document that should be updated as our CI and culture evolves.

For branches that will be merged into master, PR approval should imply the following boxes have been checked:
- [ ] The PR is marked as Ready for Review, and not in Draft mode.
- [ ] The impure tests via `nix run .#offchain:test` were run and all are passing.
- [ ] Where applicable, grammar, punctuation, and spelling within code and comments should be plausibly correct.
- [ ] The issue(s) that the work is addressing should be linked in the PR description.
- [ ] Any significant changes beyond what's clear from the linked issue(s) are documented in the PR description
