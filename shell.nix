{ compiler ? "ghc844"
, haddock ? true
, test ? true
, benchmarks ? false
, dev ? false
}:
with builtins;
let
  default = import ./default.nix {inherit benchmarks compiler dev haddock test;};
  nixpkgs = import ./nix/nixpkgs.nix { inherit compiler; };
in
  {
    free-algebras = if nixpkgs.lib.inNixShell
      then default.free-algebras.env
      else default.free-algebras;
    examples = if nixpkgs.lib.inNixShell
      then default.examples.env
      else default.examples;
  }
