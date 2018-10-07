{ mkDerivation, base, fetchgit, stdenv, tagged, tasty }:
mkDerivation {
  pname = "tasty-expected-failure";
  version = "0.11.1.1";
  src = fetchgit {
    url = "https://github.com/nomeata/tasty-expected-failure";
    sha256 = "1mdmckqmak6w01wiv8sa3bnjzzzq048r95ra0q5lpkafv0r7g749";
    rev = "a6f28fda523444dd54bdbe14da4bddb087c6661d";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [ base tagged tasty ];
  homepage = "http://github.com/nomeata/tasty-expected-failure";
  description = "Mark tasty tests as failure expected";
  license = stdenv.lib.licenses.mit;
}
