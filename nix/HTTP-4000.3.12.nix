{ mkDerivation, array, base, bytestring, deepseq, fetchgit
, httpd-shed, HUnit, mtl, network, network-uri, parsec, pureMD5
, split, stdenv, test-framework, test-framework-hunit, time
}:
mkDerivation {
  pname = "HTTP";
  version = "4000.3.12";
  src = fetchgit {
    url = "https://github.com/haskell/HTTP";
    sha256 = "0s65jg3ysjhhrnv1b3qb03piqaq0vxp7wilbk3aqq3cyyaqbgb45";
    rev = "156e5f7f1279397c1c8972cb09b61cdd0d2c263c";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    array base bytestring mtl network network-uri parsec time
  ];
  testHaskellDepends = [
    base bytestring deepseq httpd-shed HUnit mtl network network-uri
    pureMD5 split test-framework test-framework-hunit
  ];
  homepage = "https://github.com/haskell/HTTP";
  description = "A library for client-side HTTP";
  license = stdenv.lib.licenses.bsd3;
}
