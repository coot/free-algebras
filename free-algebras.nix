{ mkDerivation
, base
, containers
, free
, hedgehog
, hpack
, kan-extensions
, mtl
, stdenv
}:
mkDerivation {
  pname = "free-algebras";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base free kan-extensions mtl ];
  libraryToolDepends = [ hpack ];
  testHaskellDepends = [
    base containers free hedgehog kan-extensions mtl
  ];
  preConfigure = "hpack";
  homepage = "https://github.com/coot/free-algebras#readme";
  license = stdenv.lib.licenses.mpl20;
  enableSeparateDocOutput = false;
}
