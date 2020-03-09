{ compiler ? "ghc865" }:
with builtins;
let
  sources = import ./sources.nix {};
  config =
    { packageOverrides = super:
      let self = super.pkgs;
          lib = super.haskell.lib;
      in {
        haskell = super.haskell // {
          packages = super.haskell.packages // {
            ghc865 = super.haskell.packages.ghc865.override {
              overrides = self: super: {
                base-compat = super.callPackage ./base-compat-0.10.5.nix {};
              };
            };
          };
        };
      };
    };
  nixpkgs = import sources.nixpkgs { inherit config; };
in nixpkgs
