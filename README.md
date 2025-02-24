# Query-Driven Language Server Architecture using Second-Order Abstract Syntax

## Run `free-foil-stlc`

```console
nix develop github:deemp/query-driven-free-foil#demo
free-foil-stlc
```

## Develop the project

Clone the repository.

```console
git clone https://github.com/deemp/query-driven-free-foil
```

### Build and run with Nix

Build `free-foil-stlc`.

```console
nix build .#free-foil-stlc
```

Run with Nix.

```console
nix run .#free-foil-stlc
```

### Build and run with Cabal

Start a Nix devShell.

```console
nix develop
```

Update the Hackage index.

```console
cabal update
```

Build `free-foil-stlc`.

```console
cabal build free-foil-stlc
```

Run `free-foil-stlc`.

```console
cabal run free-foil-stlc
```

### Build with Stack

Start a Nix devShell.

```console
nix develop
```

Build `free-foil-stlc`.

```console
stack build free-foil-stlc
```

Run `free-foil-stlc`.

```console
stack run free-foil-stlc
```

### Set up direnv

Direnv caches flake devshell evaluation results.
The VS Code extension `direnv` makes other extensions aware of the environment in the devshell (dev tools, env variables, etc.).

1. Set up `direnv` ([link](https://direnv.net/#basic-installation)).
1. Install the VS Code extension `mkhl.direnv`
