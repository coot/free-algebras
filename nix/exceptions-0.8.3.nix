{ mkDerivation, base, mtl, QuickCheck, stdenv, stm
, template-haskell, test-framework, test-framework-quickcheck2
, transformers, transformers-compat
}:
mkDerivation {
  pname = "exceptions";
  version = "0.8.3";
  sha256 = "4d6ad97e8e3d5dc6ce9ae68a469dc2fd3f66e9d312bc6faa7ab162eddcef87be";
  revision = "5";
  editedCabalFile = "1kfgp41i6mfz9gjczp3flvqxfhnznd81rwldv8j05807n6mnqqii";
  libraryHaskellDepends = [
    base mtl stm template-haskell transformers transformers-compat
  ];
  testHaskellDepends = [
    base mtl QuickCheck stm template-haskell test-framework
    test-framework-quickcheck2 transformers transformers-compat
  ];
  homepage = "http://github.com/ekmett/exceptions/";
  description = "Extensible optionally-pure exceptions";
  license = stdenv.lib.licenses.bsd3;
}
