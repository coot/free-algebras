{ mkDerivation, ansi-terminal, async, base, bytestring
, concurrent-output, containers, directory, exceptions, fail
, lifted-async, mmorph, monad-control, mtl, pretty-show, primitive
, random, resourcet, semigroups, stdenv, stm, template-haskell
, text, time, transformers, transformers-base, wl-pprint-annotated
}:
mkDerivation {
  pname = "hedgehog";
  version = "1.0.1";
  sha256 = "202ea01fe1f30890307bd8c1ceaa0d24f988472e8371237271636abbf08487e1";
  revision = "1";
  editedCabalFile = "0dq3ry7py2wsiwxar11zbvm3xmifm92nx4bh61lqxzmpwyyiwnxn";
  libraryHaskellDepends = [
    ansi-terminal async base bytestring concurrent-output containers
    directory exceptions fail lifted-async mmorph monad-control mtl
    pretty-show primitive random resourcet semigroups stm
    template-haskell text time transformers transformers-base
    wl-pprint-annotated
  ];
  testHaskellDepends = [
    base containers mmorph mtl pretty-show semigroups text transformers
  ];
  homepage = "https://hedgehog.qa";
  description = "Release with confidence";
  license = stdenv.lib.licenses.bsd3;
}
