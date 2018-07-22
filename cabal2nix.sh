#!/bin/bash

hpack
cabal2nix --hpack . > ./free-algebras.nix
