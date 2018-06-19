{ mkDerivation, stdenv, base }:

mkDerivation {
  pname = "cis194";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base
  ];
  license = stdenv.lib.licenses.free;
}
