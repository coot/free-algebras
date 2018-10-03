{}:
with builtins;
let
  rev = "889c72032f8595fcd7542c6032c208f6b8033db6";
  url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
  # nix-prefetch-url --unpack
  sha256 = "0zsrcpnr0bwi81dfpmdhv8mz41lk9c1gjp6slbmagal5in14kn9c";
  config =
    { packageOverrides = super:
      let self = super.pkgs;
          lib = super.haskell.lib;
      in {
        haskell = super.haskell // {
          packages = super.haskell.packages // {
            ghc802 = super.haskell.packages.ghc802.override {
              overrides = self: super: {
                ansi-terminal = super.callPackage ./ansi-terminal-0.6.3.1.nix {};
                async = super.callPackage ./async-2.1.1.1.nix {};
                exceptions = super.callPackage ./exceptions-0.8.3.nix {};
                stm = super.callPackage ./stm-2.4.5.1.nix {};
                concurrent-output = super.callPackage ./concurrent-output-1.9.2.nix {};
              };
            };
          };
        };
      };
    };
  nixpkgs = import (fetchTarball { inherit url sha256; }) { inherit config; };
in nixpkgs
