{ mkDerivation, ansi-terminal, async, base, bytestring
, concurrent-output, containers, directory, exceptions
, lifted-async, mmorph, monad-control, mtl, pretty-show, primitive
, random, resourcet, semigroups, stdenv, stm, template-haskell
, text, th-lift, time, transformers, transformers-base, unix
, wl-pprint-annotated
}:
mkDerivation {
  pname = "hedgehog";
  version = "0.6.1";
  sha256 = "d2f94024906af37fed427fa1f03177d9a530078a2e54cfb24d7397da9807e177";
  revision = "1";
  editedCabalFile = "1fj3m5p5nm3dip93a1z7yrrq3fmqk30qgljdspia13y3lyqlcrf1";
  libraryHaskellDepends = [
    ansi-terminal async base bytestring concurrent-output containers
    directory exceptions lifted-async mmorph monad-control mtl
    pretty-show primitive random resourcet semigroups stm
    template-haskell text th-lift time transformers transformers-base
    unix wl-pprint-annotated
  ];
  testHaskellDepends = [
    base containers pretty-show semigroups text transformers
  ];
  homepage = "https://hedgehog.qa";
  description = "Hedgehog will eat all your bugs";
  license = stdenv.lib.licenses.bsd3;
}
