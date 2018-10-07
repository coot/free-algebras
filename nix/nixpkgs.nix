{}:
with builtins;
let
  rev = "6a3f5bcb061e1822f50e299f5616a0731636e4e7";
  url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
  # nix-prefetch-url --unpack
  sha256 = "1ib96has10v5nr6bzf7v8kw7yzww8zanxgw2qi1ll1sbv6kj6zpd";
  config =
    { packageOverrides = super:
      let self = super.pkgs;
          lib = super.haskell.lib;
      in {
        haskell = super.haskell // {
          packages = super.haskell.packages // {
            ghc861 = super.haskell.packages.ghc861.override {
              overrides = self: super: {
                free = super.callPackage ./free.nix {};
                semigroupoids = super.callPackage ./semigroupoids-5.3.1.nix {};
                base-orphans = super.callPackage ./base-orphans-0.8.nix {};
                polyparse = super.callPackage ./polyparse-1.12.nix {};
                haskell-src-exts = super.callPackage ./haskell-src-exts-1.20.3.nix {};
                contravariant = super.callPackage ./contravariant-1.5.nix {};
                doctest = super.callPackage ./doctest-0.16.0.1.nix {};
                unliftio-core = super.callPackage ./unliftio-core-0.1.2.0.nix {};
                unordered-containers = super.callPackage ./unordered-containers-0.2.9.0.nix {};
                concurrent-output = super.callPackage ./concurrent-output-1.10.7.nix {};
                tasty-expected-failure = super.callPackage ./tasty-expected-failure-0.11.1.1.nix {};
                QuickCheck = super.callPackage ./QuickCheck-2.12.6.1.nix {};
                ChasingBottoms = super.callPackage ./ChasingBottoms-1.3.1.5.nix {};
                test-framework-quickcheck2 = super.callPackage ./test-framework-quickcheck2-0.3.0.5.nix {};
                optparse-applicative = super.callPackage ./optparse-applicative-0.14.3.0.nix {};
                hspec-core = super.callPackage ./hspec-core-2.5.8.nix {};
                hspec = super.callPackage ./hspec-2.5.8.nix {};
                hspec-meta = super.callPackage ./hspec-meta-2.5.6.nix {};
                hspec-discover = super.callPackage ./hspec-discover-2.5.8.nix {};
                cabal-install = super.callPackage ./cabal-install-2.4.0.0.nix {};
                lifted-async = super.callPackage ./lifted-async-0.10.0.3.nix {};
                cryptohash-sha256 = super.callPackage ./cryptohash-sha256-0.11.102.0.nix {};
                hedgehog = super.callPackage ./hedgehog-0.6.1.nix {};
                HTTP = super.callPackage ./HTTP-4000.3.12.nix {};
                resolv = super.callPackage ./resolv-0.1.1.1.nix {};
                cereal = super.callPackage ./cereal-0.5.7.0.nix {};
                entropy = super.callPackage ./entropy-0.4.1.3.nix {};
                httpd-shed = super.callPackage ./httpd-shed-0.4.0.3.nix {};
              };
            };
            ghc802 = super.haskell.packages.ghc802.override {
              overrides = self: super: {
                ansi-terminal = super.callPackage ./ansi-terminal-0.6.3.1.nix {};
                async = super.callPackage ./async-2.1.1.1.nix {};
                lifted-async = super.callPackage ./lifted-async-0.9.3.3.nix {};
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
