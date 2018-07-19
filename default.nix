{ nixpkgs ? import <nixpkgs> {}
, compiler ? "ghc842"
, haddock ? true
, test ? true
, benchmarks ? false
}:

let
  lib = import ./lib.nix {};
  doHaddock = if haddock then lib.doHaddock else lib.dontHaddock;
  doTest = if test then lib.doCheck else lib.dontCheck;
  doBench = if benchmarks then lib.doBenchmark else nixpkgs.lib.id;

  drv = nixpkgs.pkgs.haskell.packages.${compiler}.callPackage ./free-algebras.nix {};
in
{ 
  free-algebras = doHaddock(doTest(doBench(lib.disableSeparateDocOutput(drv))));
}
