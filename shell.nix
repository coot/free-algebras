{ nixpkgs ? import <nixpkgs> {}
, compiler ? "ghc843"
, haddock ? true
, test ? true
, benchmarks ? false
}:

let build = import ./default.nix {inherit nixpkgs compiler haddock test;};
in if nixpkgs.lib.inNixShell
  then build.free-algebras.env
  else build.free-algebras
