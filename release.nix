{ compiler ? "ghc843" }:

with builtins;
let default = import ./default.nix
  { haddock = true;
    test = true;
    benchmarks = true; 
    dev = false;
    documentation = true;
  };
in { free-algebras = default.free-algebras; }
