{ mkDerivation, base, containers, data-fix, dlist, free, groups
, hedgehog, kan-extensions, mtl, stdenv, transformers
}:
mkDerivation {
  pname = "free-algebras";
  version = "0.0.8.1";
  sha256 = "07p1nmg88b6nvqi33q80vb2aj6svx9r33ax85ry6d7adkg83s4jz";
  libraryHaskellDepends = [
    base containers data-fix dlist free groups kan-extensions mtl
    transformers
  ];
  testHaskellDepends = [
    base containers data-fix dlist free groups hedgehog kan-extensions
    mtl transformers
  ];
  homepage = "https://github.com/coot/free-algebras#readme";
  description = "Free algebras";
  license = stdenv.lib.licenses.mpl20;
}
