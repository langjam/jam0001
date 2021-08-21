with import <nixpkgs> {};

let

  unstable = import <nixos-unstable> {};

in runCommand "dummy" {
     buildInputs = [
       haskellPackages.lentil
       wget

       # Racket
       unstable.racket
       unstable.firefox  # needed for the documentation of racket
       unstable.sqlite
    ];
} ""
