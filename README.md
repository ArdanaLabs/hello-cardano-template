# dUSD

The Ardana Dollar Stablecoin

## Using the Nix Binary Cache via Cachix

You will need to have Nix installed, with Flakes enabled.

To avoid compiling any output of the flake.nix of this project from source, you can use our Cachix:

1. Get Cachix: `nix shell nixpkgs#cachix`
2. Sign into app.cachix.org with GitHub
3. Generate a personal auth token at https://app.cachix.org/personal-auth-tokens
4. Put that token into this command and run it: `cachix authtoken <TheTokenYouJustCopied>`
5. Run `cachix use private-ardanalabs`.

You can ensure that the Cachix is being used by trying to build one of the outputs and observing that everything is downloaded, and that nothing needs to be compiled from source.


## test-plan & hello world documentation
`./docs/test-plan/test-plan.pdf` documents design decisions, testing, and acceptance criteria for the project.

`./docs/test-plan/hello-world.pdf` documents the hello world application. A simple test project to get you up and running. 

When editing any documents within ./docs/test-plan you can set up a latex pdf rendering feedback loop by running
```
# Or, if you are already inside nix-shell, run: `dusd-feedback-loop`
nix run .#feedback-loop
```

## Development workflow

### Dev shells

The onchain and offchain code have different dependencies, thus different dev shells. To launch a shell for onchain dev, run:

```
nix develop .#onchain
```

For offchain, run:

```
nix develop .#offchain
```

### Formatting

To auto-format the project tree, run:

```sh-session
nix run .#format
```

If you are in nix-shell already, you can also just run `treefmt`.

### Running checks locally

Although CI is run on every attribute of the `flake.nix` upon every commit of
every branch, you may also some of the same checks locally as follows:

```sh-session
# -L is for displaying full logs, which includes test output.
nix flake check -L
```

### Making a new package

Each project in this repository should have a `flake-module.nix` based on the
following template, at its root. This is part of the
[flake-modules-core](https://github.com/hercules-ci/flake-modules-core)
framework. Each of these `flake-module.nix` files can be thought of as a
subflake. They can also be thought of as a `default.nix` similar to what you see
in Nixpkgs next to every package. The real name for this, though, is a "flake
module".

```
{ self, ... }:
{
  perSystem = system: { config, self', inputs', ... }: {
  };
  flake = {
  };
}
```

`perSystem` is where flake attributes like `packages` or `apps` which need a
system argument like `x86_64-linux` may go.

`flake` is where any flake attribute can go, in the event of any confusion with
the framework. Anything that doesn't require a system like `x86_64-linux` goes
here.

`inputs'` and `self`' have the `system` abstracted away. For example, the
following expressions are equivalent to eachother:

- `inputs'.nixpkgs.legacyPackages`
- `self.inputs.nixpkgs.legacyPackages.x86_64-linux`

Another example of equivalence is as follows:

-  `self'.packages.hello`
-  `self.packages.x86_64-linux.hello`

