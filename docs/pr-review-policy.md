# PR Review Guidelines
This is a living document that should be updated as our CI and culture evolves.

For branches that will be merged into master, PR approval should imply the following boxes have been checked:
- [ ] The PR is marked as Ready for Review, and not in Draft mode.
- [ ] CI is passing.
- [ ] The code is up to date with the latest master.
- [ ] The impure tests via `nix run .#offchain:test` were run and all are passing.
- [ ] Where applicable, grammar, punctuation, and spelling within code and comments should be plausibly correct.
- [ ] The issue(s) that the work is addressing should be linked in the PR description.
- [ ] If there are changes in the PR that are not captured by the linked issue(s), they should be documented in the PR description.
