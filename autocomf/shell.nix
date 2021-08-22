# shell.nix
let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs {};
in
pkgs.mkShell {
  buildInputs = [
    pkgs.cabal-install
    pkgs.ghc
    pkgs.readline
  ];
}
