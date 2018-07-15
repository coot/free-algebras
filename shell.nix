let
  nixpkgs = import <nixpkgs> {};
  /* 
    nix-prefetch-git git@github.com:NixOs/nixpkgs --rev 5568c7c04b70f388bbcd412abc6f6e39ff1821c1 > nixpkgs.json
  */
  pref = builtins.fromJSON(builtins.readFile ./nixpkgs.json);
  pinned  = nixpkgs.fetchFromGitHub({
    owner  = "NixOS";
    repo   = "nixpkgs";
    rev    = pref.rev;
    sha256 = pref.sha256;
  }); 

  pkgs = import pinned {};
  /* ghc843 */
  ghc = pkgs.haskellPackages.ghc;
  # ghc861 = pkgs.haskell.compiler.ghc861;
in
  {
    free-algebras = pkgs.haskell.lib.buildStackProject {
      name = "free-algebras";
      buildInputs = [ pkgs.zlib ];
      inherit ghc;
    };
  }
