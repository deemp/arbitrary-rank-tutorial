# Query-Driven Language Server Architecture using Second-Order Abstract Syntax

## Develop the project

### Start a Nix devshell

1. Run `nix develop`

### Build with Cabal

```console
cabal build all
```

### Set up Nix and Stack

```console
stack build
```

### Set up direnv

Direnv caches flake devshell evaluation results.
The VS Code extension `direnv` makes other extensions aware of the environment in the devshell (dev tools, env variables, etc.).

1. Set up `direnv` ([link](https://direnv.net/#basic-installation)).
1. Install the VS Code extension `mkhl.direnv`
