{ mkDerivation, base, base16-bytestring, binary, bytestring
, containers, directory, fetchgit, filepath, stdenv, tasty
, tasty-hunit
}:
mkDerivation {
  pname = "resolv";
  version = "0.1.1.1";
  src = fetchgit {
    url = "https://github.com/hvr/resolv";
    sha256 = "11rlp20nb5ip7b9bn3d27k1rb5y418hm5xkswh536899bfamd91p";
    rev = "d32a9eae052b59d399db6bf2d62c0632c5d320f8";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    base base16-bytestring binary bytestring containers
  ];
  testHaskellDepends = [
    base bytestring directory filepath tasty tasty-hunit
  ];
  description = "Domain Name Service (DNS) lookup via the libresolv standard library routines";
  license = stdenv.lib.licenses.gpl2;
}
