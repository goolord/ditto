{ mkDerivation, base, containers, mtl, stdenv, text }:
mkDerivation {
  pname = "reform";
  version = "0.2.7.1";
  src = ./.;
  libraryHaskellDepends = [ base containers mtl text ];
  homepage = "http://www.happstack.com/";
  description = "reform is a type-safe HTML form generation and validation library";
  license = stdenv.lib.licenses.bsd3;
}
