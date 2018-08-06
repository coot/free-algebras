{ compiler ? "ghc843"
, haddock ? true
, test ? true
, benchmarks ? false
, dev ? true
}:
with builtins;
let
  spec = fromJSON (readFile ./nixpkgs.json);
  src = fetchTarball {
    url = "https://github.com/${spec.owner}/${spec.repo}/archive/${spec.rev}.tar.gz";
    sha256 = spec.sha256;
  };
  nixpkgs = import src {};

  pkgs = nixpkgs.haskell.packages;
  lib = nixpkgs.haskell.lib;

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
    pkgs.${compiler}.callPackage ./pkg.nix { inherit nixpkgs; }))));
  example = doDev(doHaddock(doTest(doBench(
    pkgs.${compiler}.callPackage ./example/pkg.nix { inherit free-algebras nixpkgs; }))));
in
{ inherit free-algebras example; }
