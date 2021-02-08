let
  pkgs = import <nixpkgs> {};
in
pkgs.mkShell {
  buildInputs = [
    pkgs.sbt
    pkgs.pandoc
    pkgs.entr
  ];
}
