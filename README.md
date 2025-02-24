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
cd query-driven-free-foil
```

### Set up direnv (optional)

Direnv caches flake devshell evaluation results.

Install `direnv` ([link](https://direnv.net/#basic-installation)).

Run in the repo:

```console
direnv allow
```

### Set up VS Code (optional)

Install recommended extensions (listed [here](.vscode/extensions.json)).

### Build and run with Nix

Build `free-foil-stlc`.

```console
nix build .#free-foil-stlc
```

Run `free-foil-stlc`.

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
nix run .#cabalUpdate
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
