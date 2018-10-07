{ mkDerivation, base, directory, filepath, hspec-meta, QuickCheck
, stdenv
}:
mkDerivation {
  pname = "hspec-discover";
  version = "2.5.8";
  sha256 = "5215bcf92ff6ad34ab62c5383a849c846f6d729b754028386718a1d81b053100";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base directory filepath ];
  executableHaskellDepends = [ base directory filepath ];
  testHaskellDepends = [
    base directory filepath hspec-meta QuickCheck
  ];
  testToolDepends = [ hspec-meta ];
  homepage = "http://hspec.github.io/";
  description = "Automatically discover and run Hspec tests";
  license = stdenv.lib.licenses.mit;
}
