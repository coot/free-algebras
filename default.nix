{ compiler ? "ghc843"
, haddock ? true
, test ? true
, benchmarks ? false
, dev ? true
}:
with builtins;
let
  nixpkgs = import ./nix/nixpkgs.nix { };

  lib = nixpkgs.haskell.lib;
  callPackage = nixpkgs.haskell.packages.${compiler}.callPackage;

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
    then drv: lib.enableCabalFlag drv "develop"
    else nixpkgs.lib.id;

  free-algebras = doDev(doHaddock(doTest(doBench(
    callPackage ./pkg.nix
      { inherit nixpkgs; }))));
  examples = doDev(doHaddock(doTest(doBench(
    callPackage ./examples/pkg.nix
      { inherit free-algebras nixpkgs; }))));
in
{ inherit free-algebras examples; }
