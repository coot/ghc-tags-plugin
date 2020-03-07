{ compiler ? "ghc844" }:
with builtins;
let
  rev = if   compiler == "ghc802"
          || compiler == "ghc822"
          || compiler == "ghc844"
    then "722fcbbb80b2142583e9266efe77992f8e32ac4c"
    else "57b66eb3f2a0e824c48759f2729370b1b9fd7660";
  url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
  config =
    { packageOverrides = super:
      let self = super.pkgs;
          lib = super.haskell.lib;
      in {
        haskell = super.haskell // {
          packages = super.haskell.packages // {
            ghc881 = super.haskell.packages.ghc881.override {
              overrides = self: super: {
                cabal-doctest = super.callPackage ./cabal-doctest-1.0.7.nix {};
                haskell-src-exts = super.callPackage ./haskell-src-exts-1.21.1.nix {};
                hedgehog = super.callPackage ./hedgehog-1.0.1.nix {};
              };
            };
            ghc863 = super.haskell.packages.ghc863.override {
              overrides = self: super: {
                hoopl_3_10_2_2 = self.callPackage ./hoopl-3.10.2.2.nix {};
              };
            };
            ghc862 = super.haskell.packages.ghc862.override {
              overrides = self: super: {
                hoopl_3_10_2_2 = self.callPackage ./hoopl-3.10.2.2.nix {};
              };
            };
            ghc861 = super.haskell.packages.ghc861.override {
              overrides = self: super: {
                hoopl_3_10_2_2 = self.callPackage ./hoopl-3.10.2.2.nix {};
              };
            };
            ghc802 = super.haskell.packages.ghc802.override {
              overrides = self: super: {
                ansi-terminal = super.callPackage ./ansi-terminal-0.6.3.1.nix {};
                async = super.callPackage ./async-2.1.1.1.nix {};
                lifted-async = super.callPackage ./lifted-async-0.9.3.3.nix {};
                exceptions = super.callPackage ./exceptions-0.8.3.nix {};
                stm = super.callPackage ./stm-2.4.5.1.nix {};
                concurrent-output = super.callPackage ./concurrent-output-1.9.2.nix {};
              };
            };
          };
        };
      };
    };
  nixpkgs = import (fetchTarball { inherit url; }) { inherit config; };
in nixpkgs
