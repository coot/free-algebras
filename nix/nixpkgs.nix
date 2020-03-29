{ compiler ? "ghc865" }:
with builtins;
let
  sources = import ./sources.nix {};
  config =
    { allowBroken = true;
      packageOverrides = super:
      let self = super.pkgs;
          lib = super.haskell.lib;
      in {
        haskell = super.haskell // {
          packages = super.haskell.packages // {
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

in if (compiler == "ghc802" || compiler == "ghc822" || compiler == "ghc844")
  then import sources.nixpkgs-ghc802 { inherit config; }
  else import sources.nixpkgs        { inherit config; }
