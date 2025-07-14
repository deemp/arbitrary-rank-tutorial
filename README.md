# A Tutorial Implementation of a Lambda Calculus with Parametric Predicative Arbitrary-Rank Polymorphism

`Arralac` - `Ar`bitrary-`ra`nk + `la`mbda `ca`lculus.

## Arralac

## Install `arralac`

```console
nix profile install github:deemp/arbitrary-rank-tutorial
```

### Use temporarily

Run devshell.

```console
nix develop github:deemp/arbitrary-rank-tutorial#demo
```

Run `arralac` in that devshell.

```console
arralac
```

## Use VS Code extension

Supported platforms: Linux, MacOS with `Nix` installed (NixOS isn't necessary).

1. Install `arralac` (See [Install arralac](#install-arralac)).

1. Clone and enter the repository.

    ```console
    git clone https://github.com/deemp/arbitrary-rank-tutorial
    cd arbitrary-rank-tutorial
    ```

1. Open the directory containing the VS Code extension code.
  
    ```console
    code vscode-extension
    ```

1. Type `Fn` + `F5` on `Linux` to start debugging the extension.

1. In the new VS Code window that opens automatically, find and open the `arbitrary-rank-tutorial/vscode-extension/demo/Program.arralac` file.

1. Hover over an identifier. You should see the type of that identifier.

1. Try to edit the code. You should see error messages when the program cannot be parsed or some identifiers dont exist where you mention them (unbound variables).

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

Build `arralac`.

```console
nix build .#arralac
```

Run `arralac`.

```console
nix run .#arralac -- typecheck arralac/test/data/Program1.arralac

nix run .#arralac -- interpret whnf arralac/test/data/Program1.arralac
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

Build `arralac`.

```console
cabal build arralac
```

Run `arralac`.

```console
cabal run arralac -- typecheck arralac/test/data/Program1.arralac

cabal run arralac -- interpret whnf arralac/test/data/Program1.arralac
```

### Build with Stack

Start a Nix devShell.

```console
nix develop
```

Build `arralac`.

```console
stack build arralac
```

Run `arralac`.

```console
stack run -- arralac typecheck arralac/test/data/Program1.arralac
```
