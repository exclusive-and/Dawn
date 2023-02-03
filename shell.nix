{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, containers, deepseq, exceptions
      , hashable, haskell-src-meta, hmatrix, lens, lib, mtl
      , ordered-containers, pretty-show, prettyprinter, primitive
      , template-haskell, text, transformers, unordered-containers
      }:
      mkDerivation {
        pname = "Giskard";
        version = "0.0.0";
        src = ./.;
        libraryHaskellDepends = [
          base containers deepseq exceptions hashable haskell-src-meta
          hmatrix lens mtl ordered-containers pretty-show prettyprinter
          primitive template-haskell text transformers unordered-containers
        ];
        description = "Giskard Automated Reasoning System";
        license = lib.licenses.bsd2;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
