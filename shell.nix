with import <nixpkgs> { };

{ pkgs ? import <nixpkgs> { } }:
pkgs.mkShell {
  name = "lmm";
  buildInputs = [
    pkgs.dhall
    pkgs.nodejs-14_x
    pkgs.purescript
    pkgs.spago
  ];
}
