{ mkDerivation, base, base16-bytestring, bytestring, criterion
, fetchgit, SHA, stdenv, tasty, tasty-hunit, tasty-quickcheck
}:
mkDerivation {
  pname = "cryptohash-sha256";
  version = "0.11.102.0";
  src = fetchgit {
    url = "https://github.com/hvr/cryptohash-sha256";
    sha256 = "0yqbwjmzqvg7w2vmv7ry26h17w86j2jajq2np9r8f23n49hblzhd";
    rev = "48b0a9cda7405da2b3477d40ff4505b4842520b5";
    fetchSubmodules = true;
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base bytestring ];
  testHaskellDepends = [
    base base16-bytestring bytestring SHA tasty tasty-hunit
    tasty-quickcheck
  ];
  benchmarkHaskellDepends = [ base bytestring criterion ];
  homepage = "https://github.com/hvr/cryptohash-sha256";
  description = "Fast, pure and practical SHA-256 implementation";
  license = stdenv.lib.licenses.bsd3;
}
