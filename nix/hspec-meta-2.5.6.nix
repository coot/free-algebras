{ mkDerivation, ansi-terminal, array, base, call-stack, clock
, deepseq, directory, filepath, hspec-expectations, HUnit
, QuickCheck, quickcheck-io, random, setenv, stdenv, stm, time
, transformers
}:
mkDerivation {
  pname = "hspec-meta";
  version = "2.5.6";
  sha256 = "440d3f09a9c88f5852fd654cb8f38be458c31bc0a571c612c1911eb899f2cda4";
  revision = "1";
  editedCabalFile = "0c7dq1vvk09fj6nljwwshgpkszg725hrpgnq9l2aka230sig9vz4";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    ansi-terminal array base call-stack clock deepseq directory
    filepath hspec-expectations HUnit QuickCheck quickcheck-io random
    setenv stm time transformers
  ];
  executableHaskellDepends = [
    ansi-terminal array base call-stack clock deepseq directory
    filepath hspec-expectations HUnit QuickCheck quickcheck-io random
    setenv stm time transformers
  ];
  homepage = "http://hspec.github.io/";
  description = "A version of Hspec which is used to test Hspec itself";
  license = stdenv.lib.licenses.mit;
}
