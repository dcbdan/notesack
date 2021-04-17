#include "interface.h"
#include <stddef.h> // NULL
#include <string>

// REMOVE THESE
#include <iostream>

sqlite3* db;

#define i_check(e)     \
  if(e != SQLITE_OK) { \
    sqlite3_close(db); \
    return e;          \
  }

int i_open(const char* filename) {
  int e = sqlite3_open(filename, &db);
  i_check(e);

  if(!db){
    return e;
  }
  return 0;
}

int i_open_and_init(const char* filename) {
  int e = i_open(filename);
  i_check(e);

  const char* init = 
    "CREATE TABLE View (        \
       ViewId       Text         PRIMARY KEY,\
       LocX         Integer,    \
       LocY         Integer,    \
       Selected     Integer );  \
     CREATE TABLE ViewNote (    \
       ViewId       Text,       \
       NoteId       Integer,    \
       LocX         Integer,    \
       LocY         Integer,    \
       SizeX         Integer,   \
       SizeY         Integer,   \
                                 PRIMARY KEY( ViewId, NoteId )\
     );                         \
     CREATE TABLE Note (        \
       NoteId       Integer      PRIMARY KEY,\
       Text         Text,       \
       DateCreated  Text,       \
       DateChanged  Text );"    ;

  return sqlite3_exec(db, init, NULL, NULL, NULL);
}

// close the database then close the so
int i_close() {
  return sqlite3_close(db);
}

int add_view_no_selected(const char* view_id, int loc_x, int loc_y) {
  return 1;
}

int add_view(const char* view_id, int loc_x, int loc_y, int selected) {
  return 1;
}

