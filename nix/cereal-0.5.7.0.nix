{ mkDerivation, array, base, bytestring, containers, fetchgit
, ghc-prim, QuickCheck, stdenv, test-framework
, test-framework-quickcheck2
}:
mkDerivation {
  pname = "cereal";
  version = "0.5.7.0";
  src = fetchgit {
    url = "https://github.com/GaloisInc/cereal";
    sha256 = "0mxsr142yv8n34i779pw3min2z0bvhjgb0rjxmbi2r694fc5zk6y";
    rev = "0f6176eeb260831f7b19407fde407af77abdab7b";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    array base bytestring containers ghc-prim
  ];
  testHaskellDepends = [
    base bytestring QuickCheck test-framework
    test-framework-quickcheck2
  ];
  homepage = "https://github.com/GaloisInc/cereal";
  description = "A binary serialization library";
  license = stdenv.lib.licenses.bsd3;
  doCheck = false;
}
