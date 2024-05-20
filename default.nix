{ profile ? false }:
let pkgs = import <nixpkgs> {};
    sqlite3 = dcbPkgs.sqlite;
    ghc = pkgs.haskellPackages.ghcWithPackages (pkgs: with pkgs; [vty]);
    coreutils = pkgs.coreutils;
in pkgs.stdenv.mkDerivation {
     name = "notesack";
     buildInputs = [ ghc sqlite3 coreutils ];
     builder = ./builder.sh;
     profile = profile;

     main = ./Main.hs;
     interfaceHeader = ./interface.h;
     interfaceSource = ./interface.cpp;
     notesack = ./Notesack;
   }

