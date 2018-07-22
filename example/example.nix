{ nixpkgs, mkDerivation, base, binary, bytestring, free, free-algebras
, hpack, mtl, network, transformers, stdenv
}:
mkDerivation {
  pname = "example";
  version = "0.0.1.0";
  src = nixpkgs.lib.sourceFilesBySuffices ./.
    [ ".hs" "LICENSE" "ChangeLog.md" "package.yaml" ];
  libraryHaskellDepends = [
    base binary bytestring free free-algebras mtl network transformers
  ];
  libraryToolDepends = [ hpack ];
  preConfigure = "hpack";
  license = stdenv.lib.licenses.mpl20;
  enableSeparateDocOutput = false;
}
