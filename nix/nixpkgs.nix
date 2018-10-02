{}:
with builtins;
let
  rev = "889c72032f8595fcd7542c6032c208f6b8033db6";
  url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
  # nix-prefetch-url --unpack
  sha256 = "0zsrcpnr0bwi81dfpmdhv8mz41lk9c1gjp6slbmagal5in14kn9c";
  nixpkgs = import (fetchTarball { inherit url sha256; }) { };
in nixpkgs
