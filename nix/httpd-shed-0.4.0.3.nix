{ mkDerivation, base, fetchgit, network, network-uri, stdenv }:
mkDerivation {
  pname = "httpd-shed";
  version = "0.4.0.3";
  src = fetchgit {
    url = "https://github.com/hsenag/httpd-shed/";
    sha256 = "1w8pmsnhyn18dscbzk68jhd2damhhp8ycv20dhgnw308lx9wcd5b";
    rev = "7ed0744adc5776f8a78c5f53b8147feea9aca431";
    fetchSubmodules = true;
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base network network-uri ];
  description = "A simple web-server with an interact style API";
  license = stdenv.lib.licenses.bsd3;
}
