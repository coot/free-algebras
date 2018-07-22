{ compiler ? "ghc843"
, haddock ? true
, test ? true
, benchmarks ? false
}:
with builtins;
let
  spec = fromJSON (readFile ./nixpkgs.json);
  src = fetchTarball {
    url = "https://github.com/${spec.owner}/${spec.repo}/archive/${spec.rev}.tar.gz";
    sha256 = spec.sha256;
  };
  nixpkgs = import src {};

  lib = import ./lib.nix { inherit nixpkgs; };
  pkgs = nixpkgs.haskell.packages;

  doHaddock = if haddock then lib.doHaddock else lib.dontHaddock;
  doTest = if test then lib.doCheck else lib.dontCheck;
  doBench = if benchmarks then lib.doBenchmark else nixpkgs.lib.id;

  free-algebras = pkgs.${compiler}.callPackage ./free-algebras.nix { inherit nixpkgs; };
  example = pkgs.${compiler}.callPackage ./example/example.nix { inherit free-algebras nixpkgs; };
in
{ 
  free-algebras = doHaddock(doTest(doBench(lib.disableSeparateDocOutput(free-algebras))));
  example = doHaddock(doTest(doBench(lib.disableSeparateDocOutput(example))));
}
