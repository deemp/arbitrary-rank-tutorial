# Query-Driven Language Server Architecture using Second-Order Abstract Syntax

## Usage

Run devshell.

```console
nix develop github:deemp/arbitrary-rank-tutorial#demo
```

Run `arralac` in that devshell.

```console
arralac
```

## Develop the project

### Clone the repository

```console
git clone https://github.com/deemp/arbitrary-rank-tutorial
cd arbitrary-rank-tutorial
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

Build `arbitrary-rank-tutorial`.

```console
nix build .#arbitrary-rank-tutorial
```

Run `arbitrary-rank-tutorial`.

```console
nix run .#arbitrary-rank-tutorial -- typecheck tutorial/test/data/Program1.arralac

nix run .#arbitrary-rank-tutorial -- interpret whnf tutorial/test/data/Program1.arralac
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

Build `arbitrary-rank-tutorial`.

```console
cabal build arbitrary-rank-tutorial
```

Run `arbitrary-rank-tutorial`.

```console
cabal run arbitrary-rank-tutorial -- typecheck tutorial/test/data/Program1.arralac

cabal run arbitrary-rank-tutorial -- interpret whnf tutorial/test/data/Program1.arralac
```

### Build with Stack

Start a Nix devShell.

```console
nix develop
```

Build `arbitrary-rank-tutorial`.

```console
stack build arbitrary-rank-tutorial
```

Run `arbitrary-rank-tutorial`.

```console
stack run --package arbitrary-rank-tutorial -- arralac typecheck tutorial/test/data/Program1.arralac
```
