{ mkDerivation, base, bytestring, fetchgit, stdenv, text }:
mkDerivation {
  pname = "polyparse";
  version = "1.12";
  src = fetchgit {
    url = "https://github.com/factisresearch/polyparse";
    sha256 = "1psbc1snvrw496878yhk5m4yvf1kkzj6r8a43jahnkb6az5gfqll";
    rev = "ad194f2b7fcd88a34080c03548bd8f0386a76557";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [ base bytestring text ];
  homepage = "http://code.haskell.org/~malcolm/polyparse/";
  description = "A variety of alternative parser combinator libraries";
  license = "LGPL";
}
