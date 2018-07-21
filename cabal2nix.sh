#!/bin/bash

hpack
cabal2nix --compiler ghc843 --hpack . > ./free-algebras.nix
