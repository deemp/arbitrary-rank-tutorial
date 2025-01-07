{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    devshell = {
      url = "github:deemp/devshell";
      inputs.flake-utils.follows = "flake-utils";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    flake-utils = {
      url = "github:numtide/flake-utils";
      inputs.systems.follows = "systems";
    };
    systems.url = "github:nix-systems/default";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    treefmt-nix.url = "github:numtide/treefmt-nix";
    treefmt-nix.inputs.nixpkgs.follows = "nixpkgs";
    flake-compat = {
      url = "https://flakehub.com/f/edolstra/flake-compat/1.tar.gz";
      flake = false;
    };
    free-foil = {
      url = "github:fizruk/free-foil";
      flake = false;
    };
    fcf-family = {
      url = "gitlab:lysxia/fcf-family";
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
            buildInputs = [ pkgs.makeWrapper ];
            postBuild = ''
              wrapProgram $out/bin/stack \
                --add-flags "\
                  --system-ghc \
                  --no-install-ghc \
                  --nix \
                  --no-nix-pure \
                  --nix-shell-file stack.nix \
                  --nix-path nixpkgs=${inputs.nixpkgs} \
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
          bash.vars = ''
            export LC_ALL=C.UTF-8
          '';
          jailbreakUnbreak =
            pkg:
            pkgs.haskell.lib.doJailbreak (
              pkg.overrideAttrs (_: {
                meta = { };
              })
            );
        in
        {
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

            basePackages = pkgs.haskell.packages."ghc9101";

            settings = {
              hpack =
                { super, ... }:
                {
                  custom = _: super.hpack_0_37_0;
                };
              free-foil =
                { super, ... }:
                {
                  custom = _: super.callCabal2nix "free-foil" "${inputs.free-foil}/haskell/free-foil" { };
                };
              with-utf8 =
                { super, ... }:
                {
                  custom = _: super.with-utf8_1_1_0_0;
                };
              fcf-family =
                { super, ... }:
                {
                  custom = _: super.callCabal2nix "fcf-family" "${inputs.fcf-family}/fcf-family" { };
                };
              kind-generics-th =
                { super, ... }:
                {
                  custom = _: jailbreakUnbreak super.kind-generics-th;
                };
            };

            # Development shell configuration
            devShell = {
              hlsCheck.enable = false;
              hoogle = false;
              tools = hp: {
                hlint = null;
                ghcid = null;
              };
              extraLibraries = hp: { inherit (hp) BNFC; };
            };

            # What should haskell-flake add to flake outputs?
            autoWire = [
              "packages"
              "apps"
              "checks"
            ]; # Wire all but the devShell
          };

          # Auto formatters. This also adds a flake check to ensure that the
          # source tree was auto formatted.
          treefmt.config = {
            projectRootFile = "flake.nix";
            programs = {
              nixfmt-rfc-style.enable = true;
              hlint.enable = true;
              shellcheck.enable = true;
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
              formatter = rec {
                fourmolu.excludes = [
                  "**/*.cabal"
                  "**/Setup.hs"
                ];
                hlint.excludes = fourmolu.excludes;
              };
            };
          };

          packages = mkShellApps { default = self'.packages.free-foil-stlc; };

          # buildStackProject arguments: https://github.com/NixOS/nixpkgs/blob/c7089236291045a523429e681bdaecb49bb501f3/pkgs/development/haskell-modules/generic-stack-builder.nix#L4-L11
          legacyPackages = {
            stackShell =
              { ... }:
              pkgs.haskell.lib.buildStackProject (
                let
                  hp = config.haskellProjects.default.outputs.finalPackages;
                in
                {
                  name = "stack-shell";
                  ghc = builtins.head (
                    builtins.filter (
                      x: pkgs.lib.attrsets.isDerivation x && pkgs.lib.strings.hasPrefix "ghc-" x.name
                    ) config.haskellProjects.default.outputs.devShell.nativeBuildInputs
                  );
                  buildInputs = [
                    hp.alex
                    hp.happy
                    hp.BNFC
                  ];
                }
              );
            devshell = config.haskellProjects.default.outputs.devShell;
          };

          # Default shell.
          devshells.default = {
            packagesFrom = [
              config.haskellProjects.default.outputs.devShell
              config.treefmt.build.devShell
            ];
            bash.extra = bash.vars;
            commands = {
              tools = [
                {
                  expose = true;
                  packages = {
                    stack = stack-wrapped;
                    inherit (pkgs) mdsh mdbook mdbook-linkcheck;
                    inherit (config.haskellProjects.default.outputs.finalPackages) hpack;
                  };
                }
                {
                  packages =
                    let
                      hp = config.haskellProjects.default.outputs.finalPackages;
                    in
                    {
                      cabal = hp.cabal-install;
                    };
                }
              ];

              scripts = [
                {
                  prefix = "nix run .#";
                  packages = {
                    free-foil-stlc = self'.packages.default;
                  };
                }
              ];
            };
          };
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
