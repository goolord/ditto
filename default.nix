{ mkDerivation, base, containers, mtl, stdenv, text }:
mkDerivation {
  pname = "reform";
  version = "0.2.6";
  src = ./.;
  buildDepends = [ base containers mtl text ];
  homepage = "http://www.happstack.com/";
  description = "reform is an HTML form generation and validation library";
  license = stdenv.lib.licenses.bsd3;
}
