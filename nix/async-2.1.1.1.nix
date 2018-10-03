{ mkDerivation, base, HUnit, stdenv, stm, test-framework
, test-framework-hunit
}:
mkDerivation {
  pname = "async";
  version = "2.1.1.1";
  sha256 = "cd83e471466ea6885b2e8fb60f452db3ac3fdf3ea2d6370aa1e071ebc37544e2";
  libraryHaskellDepends = [ base stm ];
  testHaskellDepends = [
    base HUnit test-framework test-framework-hunit
  ];
  homepage = "https://github.com/simonmar/async";
  description = "Run IO operations asynchronously and wait for their results";
  license = stdenv.lib.licenses.bsd3;
}
