# Build with haskell.nix;
#
# nix-build -A ghc-tags-core
# nix-build -A ghc-tags-plugin

{ compiler ? "ghc865"
}:
let compiler-nix-name = compiler;

    sources = import ./nix/sources.nix {};
    iohkNix = import sources.iohk-nix {};
    haskellNix = import sources."haskell.nix" {};
    nixpkgs = iohkNix.nixpkgs;
    haskell-nix = haskellNix.pkgs.haskell-nix;

    # package set
    pkgs = import nixpkgs { config = haskellNix.config;
                            overlays =
                              [ (_: _: { inherit ghcTagsPluginPackages; }) ];
                          };
    lib = pkgs.lib;

    # 'cleanGit' cleans a source directory based on the files known by git
    src = haskell-nix.haskellLib.cleanGit {
      name = "ghc-tags-plugin";
      src = ./.;
    };

    # unmodified packages
    projectPackages = lib.attrNames
      (haskell-nix.haskellLib.selectProjectPackages
      (haskell-nix.cabalProject { inherit src compiler-nix-name; }));

    # set GHC options
    ghcTagsPluginPackages = haskell-nix.cabalProject {
        inherit src compiler-nix-name;
        modules =
          [
            { packages =
                lib.genAttrs
                  projectPackages
                  (name: { configureFlags = [ "--ghc-option=-Werror" ]; });
            }
          ];
      };
in pkgs.ghcTagsPluginPackages
