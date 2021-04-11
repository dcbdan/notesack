with (import ~/Projects/DcbPkgs {});

derivation {
  name = "notesack";
  builder = "${dcbPkgs.bash}/bin/bash";
  args = [ ./builder.sh ];
  system = builtins.currentSystem;
  ghc = dcbPkgs.haskellPackages.ghcWithPackages (pkgs: with pkgs; [vty]); 
  coreutils = dcbPkgs.coreutils;
  sqlite = sqlite;

  main = ./Main.hs;
  interfaceh = ./interface.h;
  interfacec = ./interface.c;
}
