{ compiler   ? "ghc865",
  haddock    ? true,
  test       ? true,
  benchmarks ? false,
  dev        ? true
}:
with builtins;
let
  nixpkgs = import ./nix/nixpkgs.nix { inherit compiler; };

  lib = nixpkgs.haskell.lib;
  callCabal2nix = nixpkgs.haskell.packages.${compiler}.callCabal2nix;

  doHaddock = if haddock
    then lib.doHaddock
    else lib.dontHaddock;
  doTest = if test
    then lib.doCheck
    else lib.dontCheck;
  doBench = if benchmarks
    then lib.doBenchmark
    else nixpkgs.lib.id;
  doDev = if dev
    then drv: lib.appendConfigureFlag drv "--ghc-option -Werror"
    else nixpkgs.lib.id;
  doEnableGtpCheck = if dev
    then drv: lib.appendConfigureFlag drv "-f+gtp-check"
    else nixpkgs.lib.id;
  docNoSeprateOutput = drv: lib.overrideCabal drv (drv: { enableSeparateDocOutput = false; });

  srcFilter = src: path: type:
    let relPath = nixpkgs.lib.removePrefix (toString src + "/") (toString path);
    in
       nixpkgs.lib.hasPrefix "src"   relPath
    || nixpkgs.lib.hasPrefix "lib"   relPath
    || nixpkgs.lib.hasPrefix "test"  relPath
    || nixpkgs.lib.hasPrefix "bench" relPath
    || nixpkgs.lib.hasPrefix "app"   relPath
    || nixpkgs.lib.any
        (a: a == relPath)
        [ "Setup.hs" "CHANGELOG.md" "ghc-tags-plugin.cabal" "ghc-tags-core.cabal" "LICENSE"];

  ghc-tags-core = docNoSeprateOutput(doDev(doHaddock(doTest(doBench(
    lib.overrideCabal (callCabal2nix "ghc-tags-core" ./ghc-tags-core {})
      (drv: {src = nixpkgs.lib.cleanSourceWith { filter = srcFilter drv.src; src = drv.src; };})
  )))));

  ghc-tags-plugin = docNoSeprateOutput(doEnableGtpCheck(doDev(doHaddock(doTest(doBench(
    lib.overrideCabal (callCabal2nix "ghc-tags-plugin" ./ghc-tags-plugin { inherit ghc-tags-core; })
      (drv: {src = nixpkgs.lib.cleanSourceWith { filter = srcFilter drv.src; src = drv.src; };})
  ))))));

in { inherit ghc-tags-plugin ghc-tags-core; }
