#include "sqlite3.h"

// when these were named open / close / err, close would get
// called when compiling with
//  ghc -o notesack -threaded -outputdir build Main.hs interface.c
// even when main = return ()
// .. It didn't happen when the threaded option was removed

void i_open(const char* filename);
void i_open_and_init(const char* filename);
//void i_add_note(const char* text, int size_x, int size_y);
void i_close();
int  i_err();
