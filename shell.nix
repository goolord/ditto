{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, containers, mtl, stdenv, semigroups, text, cabal-install }:
      mkDerivation {
        pname = "reform";
        version = "0.2.7.1";
        src = ./.;
        libraryHaskellDepends = [ base containers mtl semigroups text cabal-install ];
        homepage = "http://www.happstack.com/";
        description = "reform is a type-safe HTML form generation and validation library";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
