{ mkDerivation, array, async, base, base16-bytestring, binary
, bytestring, Cabal, containers, cryptohash-sha256, deepseq
, directory, echo, edit-distance, filepath, hackage-security
, hashable, HTTP, mtl, network, network-uri, parsec, pretty
, process, random, resolv, stdenv, stm, tar, text, time, unix
, zip-archive, zlib
}:
mkDerivation {
  pname = "cabal-install";
  version = "2.4.0.0";
  sha256 = "1329e9564b736b0cfba76d396204d95569f080e7c54fe355b6d9618e3aa0bef6";
  revision = "1";
  editedCabalFile = "0cni9i6f6kbfl6f5gypb9ky94ib1w6d0nkd05j4lmrjadnxss49a";
  isLibrary = false;
  isExecutable = true;
  setupHaskellDepends = [ base Cabal filepath process ];
  executableHaskellDepends = [
    array async base base16-bytestring binary bytestring Cabal
    containers cryptohash-sha256 deepseq directory echo edit-distance
    filepath hackage-security hashable HTTP mtl network network-uri
    parsec pretty process random resolv stm tar text time unix
    zip-archive zlib
  ];
  doCheck = false;
  postInstall = ''
    mkdir $out/etc
    mv bash-completion $out/etc/bash_completion.d
  '';
  homepage = "http://www.haskell.org/cabal/";
  description = "The command-line interface for Cabal and Hackage";
  license = stdenv.lib.licenses.bsd3;
}
