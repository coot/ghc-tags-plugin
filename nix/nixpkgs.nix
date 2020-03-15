{ compiler ? "ghc865" }:
with builtins;
let
  sources = import ./sources.nix {};
  config =
    { allowBroken = true;
      packageOverrides = super:
      let self = super.pkgs;
          lib = super.haskell.lib;
      in {
        haskell = super.haskell // {
          packages = super.haskell.packages // {
            ghc865 = super.haskell.packages.ghc865.override {
              overrides = self: super: {
                pipes-text  = super.callPackage ./pipes-text.nix {};
                time-compat = super.callPackage ./time-compat-1.9.3.nix {};
              };
            };
            ghc883 = super.haskell.packages.ghc865.override {
              overrides = self: super: {
                pipes-text  = super.callPackage ./pipes-text.nix {};
                time-compat = super.callPackage ./time-compat-1.9.3.nix {};
              };
            };
          };
        };
      };
    };
  nixpkgs = import sources.nixpkgs { inherit config; };
in nixpkgs
