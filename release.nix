{ compiler }:

with builtins;
let default = import ./default.nix
      { haddock    = true;
        test       = true;
        benchmarks = true;
        dev        = true;
        inherit compiler;
      };
in { ghc-tags-core = default.ghc-tags-core;
     ghc-tags-plugin = default.ghc-tags-plugin;
   }
