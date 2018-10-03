{ mkDerivation, ansi-terminal, async, base, directory, exceptions
, process, stdenv, stm, terminal-size, text, transformers, unix
}:
mkDerivation {
  pname = "concurrent-output";
  version = "1.9.2";
  sha256 = "3bc2423adb5cdae14278e718b1335363cc21cd74b370d47dc4e07d2287b2d9f3";
  libraryHaskellDepends = [
    ansi-terminal async base directory exceptions process stm
    terminal-size text transformers unix
  ];
  description = "Ungarble output from several threads or commands";
  license = stdenv.lib.licenses.bsd2;
}
