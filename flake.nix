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
    fcf-family = {
      url = "gitlab:lysxia/fcf-family";
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
          stack-wrapped = pkgs.symlinkJoin {
            name = "stack"; # will be available as the usual `stack` in terminal
            paths = [ pkgs.stack ];
            meta = pkgs.stack.meta;
            version = pkgs.stack.version;
            buildInputs = [ pkgs.makeWrapper ];
            postBuild = ''
              wrapProgram $out/bin/stack \
                --add-flags "\
                  --no-nix \
                  --system-ghc \
                  --no-install-ghc \
                "
            '';
          };

          mkShellApps = lib.mapAttrs (
            name: value:
            if !(lib.isDerivation value) && lib.isAttrs value then
              pkgs.writeShellApplication (value // { inherit name; })
            else
              value
          );

          jailbreakUnbreak =
            pkg:
            pkgs.haskell.lib.doJailbreak (
              pkg.overrideAttrs (_: {
                meta = { };
              })
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
              overrides =
                self: super:
                let
                  packageFromHackage =
                    pkg: ver: sha256:
                    super.callHackageDirect { inherit pkg ver sha256; } { };
                in
                {
                  free-foil = super.callCabal2nix "free-foil" "${inputs.free-foil}/haskell/free-foil" { };
                  with-utf8 = super.with-utf8_1_1_0_0;
                  fcf-family = super.callCabal2nix "fcf-family" "${inputs.fcf-family}/fcf-family" { };
                  kind-generics-th = jailbreakUnbreak super.kind-generics-th;
                  BNFC = super.callCabal2nix "BNFC" "${inputs.bnfc}/source" { };

                  # Simply use Hackage instead of overriding all-cabal-hashes (~2GB unpacked)
                  # https://github.com/NixOS/nixpkgs/blob/21d55dd87e040944379bfe0574d9e24caf3dec20/pkgs/development/haskell-modules/make-package-set.nix#L28
                  alex = packageFromHackage "alex" "3.5.2.0" "sha256-hTkBDe30UkUVx1MTa4BjpYK5nyYlULCylZEniW6sSnA=";
                  happy = packageFromHackage "happy" "2.1.5" "sha256-rM6CpEFZRen8ogFIOGjKEmUzYPT7dor/SQVVL8RzLwE=";
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
                alex = default;
                happy = default;
                happy-lib = default;
                BNFC = default;
                hedgehog = default;
                with-utf8 = default;
                free-foil = default;
                fcf-family = default;
                kind-generics = default;
                free-foil-stlc.extraBuildTools = [
                  buildTools.alex
                  buildTools.happy
                  buildTools.bnfc
                ];
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

          configDefault = {
            outputs = config.haskellProjects.default.outputs;
            haskellPackages = configDefault.outputs.finalPackages;
            inherit (configDefault.outputs) devShell;
          };

          buildTools = {
            inherit (configDefault.haskellPackages) alex happy;
            bnfc = configDefault.haskellPackages.BNFC;

            cabal = pkgs.cabal-install;

            stack = stack-wrapped;

            ghc = builtins.head (
              builtins.filter (
                x: pkgs.lib.attrsets.isDerivation x && pkgs.lib.strings.hasPrefix "ghc-" x.name
              ) configDefault.devShell.nativeBuildInputs
            );
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
                  self'.packages.cabalUpdate
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
                    packages = {
                      hpack = haskellPackages.hpack_0_37_0;

                      inherit (haskellPackages) haskell-language-server;

                      inherit (buildTools)
                        alex
                        happy
                        bnfc
                        cabal
                        stack
                        ghc
                        ;
                    };
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
              buildInputs = with buildTools; [
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
