{ nixpkgs ? import <nixpkgs> {} }:
let
  lib = nixpkgs.haskell.lib;
  disableSeparateDocOutput = drv: lib.overrideCabal drv (drv: { enableSeparateDocOutput = false; });
in
  lib // { disableSeparateDocOutput = disableSeparateDocOutput; }
