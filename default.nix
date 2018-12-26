{ mkDerivation, base, containers, mtl, semigroups, stdenv, text }:
mkDerivation {
  pname = "reform";
  version = "0.2.7.2";
  src = ./.;
  libraryHaskellDepends = [ base containers mtl semigroups text ];
  homepage = "http://www.happstack.com/";
  description = "reform is a type-safe HTML form generation and validation library";
  license = stdenv.lib.licenses.bsd3;
}
