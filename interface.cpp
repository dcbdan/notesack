#include "interface.h"
#include <stddef.h> // NULL
#include <string>

// REMOVE THESE
#include <iostream>

int error = 0;
sqlite3* db;

void i_open(const char* filename) {
  error = 0;

  int e = sqlite3_open(filename, &db);
  if(e != SQLITE_OK) {
    sqlite3_close(db);
    error = e;
    return;
  }

  if(!db){
    error = e;
    return;
  }
}

void i_open_and_init(const char* filename) {
  i_open(filename);

  if(error) {
    return;
  }

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

  int e = sqlite3_exec(db, init, NULL, NULL, NULL);
  if(e != SQLITE_OK) {
    sqlite3_close(db);
    error = 1;
    return;
  }
}

// close the database then close the so
void i_close() {
  error = 0;

  int e = sqlite3_close(db);
  if(e != SQLITE_OK) {
    error = e;
  }
}

int i_err() {
  return error;
}

