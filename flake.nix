{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    devshell = {
      url = "github:deemp/devshell";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    systems.url = "github:nix-systems/default";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    free-foil = {
      url = "github:fizruk/free-foil";
      flake = false;
    };
    bnfc = {
      url = "github:deemp/bnfc";
      flake = false;
    };
    cache-nix-action = {
      url = "github:nix-community/cache-nix-action";
      flake = false;
    };
  };

  outputs =
    inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      systems = import inputs.systems;
      imports = [
        inputs.haskell-flake.flakeModule
        inputs.treefmt-nix.flakeModule
        inputs.devshell.flakeModule
      ];
      perSystem =
        {
          self',
          system,
          lib,
          config,
          pkgs,
          ...
        }:
        let
          mkShellApps = lib.mapAttrs (
            name: value:
            if !(lib.isDerivation value) && lib.isAttrs value then
              pkgs.writeShellApplication (value // { inherit name; })
            else
              value
          );

          haskellPackages = pkgs.haskell.packages."ghc9101";

          # Our only Haskell project. You can have multiple projects, but this template
          # has only one.
          # See https://github.com/srid/haskell-flake/blob/master/example/flake.nix
          haskellProjects.default = {
            # To avoid unnecessary rebuilds, we filter projectRoot:
            # https://community.flake.parts/haskell-flake/local#rebuild
            projectRoot = builtins.toString (
              lib.fileset.toSource {
                root = ./.;
                fileset = lib.fileset.unions [
                  ./free-foil-stlc
                  ./free-foil-exercises
                  ./cabal.project
                  ./README.md
                ];
              }
            );

            basePackages = haskellPackages.override {
              # If need to remove dependency bounds
              # https://github.com/balsoft/lambda-launcher/blob/c4621b41989ff63b7241cf2a65335b4880f532e0/flake.nix#L17-L23
              overrides =
                self: super:
                let
                  # Simply use Hackage instead of overriding all-cabal-hashes (~2GB unpacked)
                  # https://github.com/NixOS/nixpkgs/blob/21d55dd87e040944379bfe0574d9e24caf3dec20/pkgs/development/haskell-modules/make-package-set.nix#L28
                  packageFromHackage =
                    pkg: ver: sha256:
                    super.callHackageDirect { inherit pkg ver sha256; } { };
                in
                {
                  # direct dependencies

                  free-foil = super.callCabal2nix "free-foil" "${inputs.free-foil}/haskell/free-foil" { };
                  with-utf8 = super.with-utf8_1_1_0_0;

                  # build tools

                  alex = packageFromHackage "alex" "3.5.2.0" "sha256-hTkBDe30UkUVx1MTa4BjpYK5nyYlULCylZEniW6sSnA=";
                  BNFC = super.callCabal2nix "BNFC" "${inputs.bnfc}/source" { };
                  happy = packageFromHackage "happy" "2.1.5" "sha256-rM6CpEFZRen8ogFIOGjKEmUzYPT7dor/SQVVL8RzLwE=";
                  hpack = super.hpack_0_37_0;

                  # indirect dependencies

                  ## needed by free-foil

                  fcf-family =
                    packageFromHackage "fcf-family" "0.2.0.2"
                      "sha256-tfoOpYoHmt++8cXfr+PwjA6A/DohTA0yYJqigmqqL6U=";

                  ## needed by happy

                  happy-lib =
                    packageFromHackage "happy-lib" "2.1.5"
                      "sha256-XzWzDiJUBTxuliE5RN6MOeIdKzQQD1NurDrtZ/dW4OQ=";
                };
            };

            settings =
              let
                default = {
                  haddock = false;
                  check = false;
                };
              in
              {
                # local packages

                free-foil-stlc = default // {
                  extraBuildTools = with devTools; [
                    alex
                    happy
                    bnfc
                  ];
                };

                # direct dependencies

                free-foil = {
                  check = false;
                };
                with-utf8 = default;

                # build tools

                alex = default;
                BNFC = default;
                happy = default;
                hpack = default;

                # indirect dependencies

                ## needed by free-foil

                kind-generics = default;

                ## needed by happy

                happy-lib = default;
              };

            # Development shell configuration
            devShell = {
              hlsCheck.enable = false;
              hoogle = false;
              tools = hp: {
                cabal-install = null;
                hlint = null;
                haskell-language-server = null;
                ghcid = null;
              };
            };

            # What should haskell-flake add to flake outputs?
            autoWire = [
              "packages"
              "apps"
              "checks"
            ]; # Wire all but the devShell
          };

          devTools =
            let
              output = config.haskellProjects.default.outputs;
              wrapTool =
                pkgsName: pname: flags:
                let
                  pkg = pkgs.${pkgsName};
                in
                pkgs.symlinkJoin {
                  name = pname;
                  paths = [ pkg ];
                  meta = pkg.meta;
                  version = pkg.version;
                  buildInputs = [ pkgs.makeWrapper ];
                  postBuild = ''
                    wrapProgram $out/bin/${pname} \
                      --add-flags "${flags}"
                  '';
                };
            in
            {
              inherit (output.finalPackages) alex happy hpack;

              bnfc = output.finalPackages.BNFC;

              cabal = wrapTool "cabal-install" "cabal" "-v0";
              stack = wrapTool "stack" "stack" "--silent --no-nix --system-ghc --no-install-ghc";

              ghc = builtins.head (
                builtins.filter (
                  x: pkgs.lib.attrsets.isDerivation x && pkgs.lib.strings.hasPrefix "ghc-" x.name
                ) output.devShell.nativeBuildInputs
              );

              inherit (haskellPackages) haskell-language-server;
            };

          # Auto formatters. This also adds a flake check to ensure that the
          # source tree was auto formatted.
          treefmt.config = {
            projectRootFile = "flake.nix";
            programs = {
              nixfmt.enable = true;
              shellcheck.enable = true;
              # TODO update to "3.24.4" in nixpkgs
              # TODO suggest treefmt-nix use the latexindent from perlpackages to not load the texlive
              # https://github.com/numtide/treefmt-nix/blob/main/programs/latexindent.nix
              # https://github.com/cmhughes/latexindent.pl
              # latexindent.enable = true;
              fourmolu = {
                enable = true;
                ghcOpts = [
                  "NoPatternSynonyms"
                  "CPP"
                ];
              };
              prettier.enable = true;
            };
            settings = {
              global.excludes = [
                "**.{gitignore,png,pdf,cabal,project,cf,bib}"
                "free-foil-stlc/src/Language/STLC/Syntax/*"
              ];
            };
          };

          # TODO generateOptparseApplicativeCompletions
          packages = mkShellApps {
            default = self'.packages.free-foil-stlc;
            inherit
              (import "${inputs.cache-nix-action}/saveFromGC.nix" {
                inherit pkgs inputs;
                inputsExclude = [
                  inputs.devshell
                  inputs.treefmt-nix
                ];
                derivations = [
                  self'.packages.default
                  self'.devShells.ci-build
                ];
              })
              saveFromGC
              ;
          };

          devshells = {
            default = {
              commands = {
                tools = [
                  {
                    expose = true;
                    packages = devTools;
                  }
                ];

                scripts = [
                  {
                    prefix = "nix run .#";
                    packages = {
                      inherit (self'.packages) free-foil-stlc;
                    };
                  }
                  {
                    prefix = "nix fmt";
                    help = "Format files.";
                  }
                ];
              };
            };
            demo = {
              # TODO source optparse-applicative completions in shellHook
              commands = {
                tools = [
                  {
                    expose = true;
                    packages = {
                      inherit (self'.packages) free-foil-stlc;
                    };
                  }
                ];
              };
            };
          };

          devShells = {
            # If need C libraries, use LD_LIBRARY_PATH + pkgs.lib.makeLibraryPath.
            # https://docs.haskellstack.org/en/stable/topics/nix_integration/#supporting-both-nix-and-non-nix-developers
            # 
            # In stack.yaml, use ghc-* resolver that matches ghcWithPackages available in the devShell.
            # https://discourse.nixos.org/t/using-yesod-devel-without-building-dependencies/5827/3
            # Otherwise, stack will use its resolver and build missing packages.
            # Check it with `rm -r $(stack path --stack-root); stack build --dry-run`.
            ci-build = pkgs.mkShell {
              buildInputs = with devTools; [
                cabal
                stack
                ghc
                alex
                happy
                bnfc
              ];
            };
          };
        in
        {
          inherit
            treefmt
            devshells
            haskellProjects
            packages
            devShells
            ;
        };
    };

  nixConfig = {
    extra-substituters = [
      "https://nix-community.cachix.org"
      "https://cache.iog.io"
    ];
    extra-trusted-public-keys = [
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    ];
  };
}
