
# Package       : Giskard.nix
# Description   : Build Giskard with Nix-managed dependencies using Cabal-to-Nix

{ nixpkgs ? import <nixpkgs> {} }:

nixpkgs.haskell.packages.ghc94.callCabal2nix "giskard" ./. {}

