{ mkDerivation, base, stdenv }:
mkDerivation {
  pname = "ansi-terminal";
  version = "0.6.3.1";
  sha256 = "458f98e0c9217897f0ff07f730cfc3ed380089936fb31942aec31bb336608095";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base ];
  homepage = "https://github.com/feuerbach/ansi-terminal";
  description = "Simple ANSI terminal support, with Windows compatibility";
  license = stdenv.lib.licenses.bsd3;
}
