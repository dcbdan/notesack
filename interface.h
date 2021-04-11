#include "libFile.h"
#include "sqlite3.h"

// when these were named open / close / err, close would get
// called when compiling with
//  ghc -o notesack -threaded -outputdir build Main.hs interface.c
// even when main = return ()
// .. It didn't happen when the threaded option was removed

void i_open(const char* filename);
void i_close();
int  i_err();
