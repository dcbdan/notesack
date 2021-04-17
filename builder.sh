source $stdenv/setup
mkdir $out
cd $out
mkdir build
mkdir src

cd src
cp $main       Main.hs
cp $interfaceHeader interface.h
cp $interfaceSource interface.cpp
cp $misc Misc.hs

ghc -o ../notesack -threaded -lsqlite3 -lstdc++ -outputdir ../build Main.hs interface.cpp

