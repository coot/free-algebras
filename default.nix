{ nixpkgs ? import <nixpkgs> {}
, compiler ? "ghc842"
}:

{ 
  free-algebras = nixpkgs.pkgs.haskell.packages.${compiler}.callPackage ./free-algebras.nix {};
}
