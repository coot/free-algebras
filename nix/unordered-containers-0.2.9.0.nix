{ mkDerivation, base, bytestring, ChasingBottoms, containers
, criterion, deepseq, deepseq-generics, fetchgit, hashable, hashmap
, HUnit, mtl, QuickCheck, random, stdenv, test-framework
, test-framework-hunit, test-framework-quickcheck2
}:
mkDerivation {
  pname = "unordered-containers";
  version = "0.2.9.0";
  src = fetchgit {
    url = "https://github.com/tibbe/unordered-containers";
    sha256 = "1900jzqri2b64fxj5rgm5xj6yc3w50z6cp2fjxdnwd89mxsfbkfh";
    rev = "efa43a2ab09dc6eb72893d12676a8e188cb4ca63";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [ base deepseq hashable ];
  testHaskellDepends = [
    base ChasingBottoms containers hashable HUnit QuickCheck
    test-framework test-framework-hunit test-framework-quickcheck2
  ];
  benchmarkHaskellDepends = [
    base bytestring containers criterion deepseq deepseq-generics
    hashable hashmap mtl random
  ];
  homepage = "https://github.com/tibbe/unordered-containers";
  description = "Efficient hashing-based container types";
  license = stdenv.lib.licenses.bsd3;
}
