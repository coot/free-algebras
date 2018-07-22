{ nixpkgs, mkDerivation, base, binary, bytestring, free, free-algebras
, mtl, network, transformers, stdenv
}:
mkDerivation {
  pname = "example";
  version = "0.1.0.0";
  src = nixpkgs.lib.sourceFilesBySuffices ./.
    [ ".hs" "LICENSE" "ChangeLog.md" ".cabal" ];
  libraryHaskellDepends = [
    base binary bytestring free free-algebras mtl network transformers
  ];
  license = stdenv.lib.licenses.mpl20;
}
