{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    nixpkgs-old.url = "github:nixos/nixpkgs/c792c60b8a97daa7efe41a6e4954497ae410e0c1";
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
    all-cabal-hashes = {
      url = "github:commercialhaskell/all-cabal-hashes/hackage";
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

          jailbreakUnbreak =
            pkg:
            pkgs.haskell.lib.doJailbreak (
              pkg.overrideAttrs (_: {
                meta = { };
              })
            );

          haskellPackages = pkgs.haskell.packages."ghc9101";

          basePackagesDefault =
            overrides:
            haskellPackages.override {
              all-cabal-hashes = inputs.all-cabal-hashes;
              inherit overrides;
            };

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

            basePackages = basePackagesDefault (
              self: super: {
                free-foil = super.callCabal2nix "free-foil" "${inputs.free-foil}/haskell/free-foil" { };
                with-utf8 = super.with-utf8_1_1_0_0;
                fcf-family = super.callCabal2nix "fcf-family" "${inputs.fcf-family}/fcf-family" { };
                kind-generics-th = jailbreakUnbreak super.kind-generics-th;
              }
            );

            packages = {
              alex.source = "3.5.2.0";
              happy.source = "2.1.5";
              happy-lib.source = "2.1.5";
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
              extraLibraries = hp: { inherit (hp) BNFC alex happy; };
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
            inherit (configDefault.outputs) finalPackages devShell;
          };
        in
        {
          inherit haskellProjects;

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
              latexindent.enable = true;
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

          packages = mkShellApps { default = self'.packages.free-foil-stlc; };

          legacyPackages = {
            stackShell =
              { ... }:
              # buildStackProject arguments: https://github.com/NixOS/nixpkgs/blob/7395957192312b3db2ea4e8e29f58a557d17bb45/pkgs/development/haskell-modules/generic-stack-builder.nix#L12-L20
              pkgs.haskell.lib.buildStackProject ({
                name = "stack-shell";
                ghc = builtins.head (
                  builtins.filter (
                    x: pkgs.lib.attrsets.isDerivation x && pkgs.lib.strings.hasPrefix "ghc-" x.name
                  ) configDefault.devShell.nativeBuildInputs
                );
                buildInputs =
                  let
                    hp = configDefault.finalPackages;
                  in
                  [
                    hp.alex
                    hp.happy
                    hp.BNFC
                  ];
              });
          };

          # Default shell.
          devshells.default = {
            packagesFrom = [
              configDefault.devShell
              config.treefmt.build.devShell
            ];
            commands = {
              tools = [
                {
                  expose = true;
                  packages = {
                    inherit (pkgs) mdsh mdbook mdbook-linkcheck;
                    inherit (configDefault.finalPackages) alex happy BNFC;
                    hpack = haskellPackages.hpack_0_37_0;
                    inherit (haskellPackages) haskell-language-server;

                    # cabal-install 3.14.1.0
                    # if you get `<...>alex: cannot execute: required file not found`,
                    # `rm -rf ~/.cabal/store/ghc-9.10.1*`
                    inherit (haskellPackages) cabal-install;

                    # https://github.com/haskell/cabal/issues/10717#issuecomment-2571718442
                    # cabal-install 3.12.1.0
                    # requires running cabal v1-build in package directories
                    # inherit (inputs.nixpkgs-old.legacyPackages.${system}) cabal-install;

                    # it just works
                    stack = stack-wrapped;
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
