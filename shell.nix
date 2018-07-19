{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc842", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage (import ./free-algebras.nix) {});

in

  if pkgs.lib.inNixShell then drv.env else drv
