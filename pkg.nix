{ mkDerivation
, base
, constraints
, containers
, data-fix
, dlist
, free
, groups
, hedgehog 
, hpack 
, kan-extensions
, mtl
, natural-numbers
, nixpkgs
, stdenv
, transformers
}:
let
  lib = nixpkgs.lib;
  srcFilter = src: path: type:
    let relPath = lib.removePrefix (toString src + "/") (toString path);
    in 
       lib.hasPrefix "src" relPath
    || lib.hasPrefix "test" relPath
    || lib.any
        (a: a == relPath)
        [ "Setup.hs" "cabal.project" "ChangeLog.md" "package.yaml" "LICENSE"];
in
mkDerivation {
  pname = "free-algebras";
  version = "0.0.4.0";
  src = lib.cleanSourceWith { filter = srcFilter ./.; src = ./.; };
  libraryHaskellDepends = [
    base
    constraints
    data-fix
    dlist
    free
    groups
    kan-extensions
    mtl
    natural-numbers
    transformers
  ];
  libraryToolDepends = [ hpack ];
  testHaskellDepends = [
    base
    containers
    free
    hedgehog
    kan-extensions
    mtl
    transformers
  ];
  preConfigure = "hpack";
  homepage = "https://github.com/coot/free-algebras#readme";
  license = stdenv.lib.licenses.mpl20;
  enableSeparateDocOutput = false;
}
