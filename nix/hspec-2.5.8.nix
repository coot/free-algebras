{ mkDerivation, base, hspec-core, hspec-discover
, hspec-expectations, QuickCheck, stdenv
}:
mkDerivation {
  pname = "hspec";
  version = "2.5.8";
  sha256 = "595b675dad0252282a2a090d257436a3afc0abdab6ccfd2ba0967d2a43263318";
  libraryHaskellDepends = [
    base hspec-core hspec-discover hspec-expectations QuickCheck
  ];
  homepage = "http://hspec.github.io/";
  description = "A Testing Framework for Haskell";
  license = stdenv.lib.licenses.mit;
}
