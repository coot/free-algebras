{ mkDerivation, array, base, stdenv }:
mkDerivation {
  pname = "stm";
  version = "2.4.5.1";
  sha256 = "6cf0c280062736c9980ba1c2316587648b8e9d4e4ecc5aed16a41979c0a3a3f4";
  libraryHaskellDepends = [ array base ];
  homepage = "https://wiki.haskell.org/Software_transactional_memory";
  description = "Software Transactional Memory";
  license = stdenv.lib.licenses.bsd3;
}
