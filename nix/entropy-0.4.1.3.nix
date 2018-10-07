{ mkDerivation, base, bytestring, Cabal, directory, filepath
, process, stdenv, unix
}:
mkDerivation {
  pname = "entropy";
  version = "0.4.1.3";
  sha256 = "510aebda134d1c835250bce8e5e7008fe54a929b05ced6a45121be488935a91c";
  setupHaskellDepends = [ base Cabal directory filepath process ];
  libraryHaskellDepends = [ base bytestring unix ];
  homepage = "https://github.com/TomMD/entropy";
  description = "A platform independent entropy source";
  license = stdenv.lib.licenses.bsd3;
}
