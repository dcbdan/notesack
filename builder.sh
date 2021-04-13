source $stdenv/setup
mkdir $out
cd $out
mkdir build
mkdir src

cd src
cp $main       Main.hs
cp $interfaceh interface.h
cp $interfacec interface.c

ghc -o ../notesack -threaded -lsqlite3 -outputdir ../build Main.hs interface.c

