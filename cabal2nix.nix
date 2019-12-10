{ mkDerivation, base, containers, mtl, stdenv, text }:
mkDerivation {
  pname = "ditto";
  version = "0.4.1";
  src = ./.;
  libraryHaskellDepends = [ base containers mtl text ];
  description = "ditto is a type-safe HTML form generation and validation library";
  license = stdenv.lib.licenses.bsd3;
}
