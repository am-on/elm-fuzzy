{
  pkgs ? import <nixpkgs> {}
}:

with pkgs;
stdenv.mkDerivation {
  name = "elm-fuzzy";
  buildInputs = [
    elmPackages.elm-analyse
    elmPackages.elm-format
    elmPackages.elm
  ];
}
