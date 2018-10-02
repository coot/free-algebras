{ nixpkgs
, mkDerivation
, base
, binary
, bytestring
, constraints
, dlist
, free
, free-algebras
, groups
, mtl
, network
, transformers
, stdenv
}:
mkDerivation {
  pname = "free-algebras-examples";
  version = "0.0.3.0";
  src = nixpkgs.lib.sourceFilesBySuffices ./.
    [ ".hs" "LICENSE" "ChangeLog.md" "free-algebras-examples.cabal" ];
  libraryHaskellDepends = [
    base
    binary bytestring
    constraints
    dlist
    free
    free-algebras
    groups
    mtl
    network
    transformers
  ];
  libraryToolDepends = [ ];
  license = stdenv.lib.licenses.mpl20;
  enableSeparateDocOutput = false;
}
