nix run .#offchain:hello-world-cli:test \
    && nix run .#offchain:hello-world-api:test \
    && nix run .#offchain:hello-world-browser:test \
    && nix run .#offchain:hello-world-browser:serve 
