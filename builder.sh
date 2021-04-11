export PATH="$coreutils/bin:$ghc/bin"
export libsqlite="$sqlite/lib/libsqlite3.so"
mkdir $out
cd $out
mkdir build
cp $main Main.hs
cp $interfaceh interface.h
cp $interfacec interface.c
ln -s $sqlite/include/sqlite3.h .

echo "// This file was automatically generated //" >> libFile.h
echo "const char* sqlite3so = \"$libsqlite\";"     >> libFile.h

ghc -o notesack -threaded -outputdir build Main.hs interface.c
