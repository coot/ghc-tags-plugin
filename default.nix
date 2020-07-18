# Build with haskell.nix;
#
# nix-build -A ghc-tags-core
# nix-build -A ghc-tags-plugin

{ # Fetch the latest haskell.nix and import its default.nix
  haskellNix ? import (builtins.fetchTarball "https://github.com/input-output-hk/haskell.nix/archive/master.tar.gz") {}

# haskell.nix provides access to the nixpkgs pins which are used by our CI,
# hence you will be more likely to get cache hits when using these.
# But you can also just use your own, e.g. '<nixpkgs>'.
, nixpkgsSrc ? haskellNix.sources.nixpkgs-2003

# haskell.nix provides some arguments to be passed to nixpkgs, including some
# patches and also the haskell.nix functionality itself as an overlay.
, nixpkgsArgs ? haskellNix.nixpkgsArgs

# import nixpkgs with overlays
, nixpkgs ? import nixpkgsSrc nixpkgsArgs
, compiler ? "ghc865"
}:
let compiler-nix-name = compiler;
    lib = nixpkgs.lib;
    # 'cleanGit' cleans a source directory based on the files known by git
    src = nixpkgs.haskell-nix.haskellLib.cleanGit {
      name = "ghc-tags-plugin";
      src = ./.;
    };
    projectPackages = lib.attrNames (nixpkgs.haskell-nix.haskellLib.selectProjectPackages
      (nixpkgs.haskell-nix.cabalProject { inherit src compiler-nix-name; }));
in nixpkgs.haskell-nix.cabalProject {
      inherit src compiler-nix-name;
      modules =
        [ {
            packages = lib.genAttrs projectPackages
              (name: { configureFlags = [ "--ghc-option=-Werror" ]; });
          }
        ];
    }
