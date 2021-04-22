source $stdenv/setup
mkdir $out
cd $out
mkdir build
mkdir src

cd src
cp $main       Main.hs
cp $interfaceHeader interface.h
cp $interfaceSource interface.cpp
mkdir Notesack
cp $notesack/* Notesack

if [ "$profile" = 1 ] ; then
  ghc -prof -fprof-auto -o ../notesack -threaded -lsqlite3 -lstdc++ -outputdir ../build Main.hs interface.cpp
else 
  ghc -o ../notesack -threaded -lsqlite3 -lstdc++ -outputdir ../build Main.hs interface.cpp
fi
