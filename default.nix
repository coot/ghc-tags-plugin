{ pkgs ? import <nixpkgs> (import (builtins.fetchTarball https://github.com/input-output-hk/haskell.nix/archive/master.tar.gz))
, haskellCompiler ? "ghc865"
}:
  pkgs.haskell-nix.cabalProject {
    src = pkgs.haskell-nix.haskellLib.cleanGit { src = ./.; };
    ghc = pkgs.buildPackages.pkgs.haskell-nix.compiler.${haskellCompiler};
  }
