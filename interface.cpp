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
                                 PRIMARY KEY( ViewId, NoteId )\
     );                         \
     CREATE TABLE Note (        \
       NoteId       Integer      PRIMARY KEY,\
       Text         Text,       \
       DateCreated  Text,       \
       DateChanged  Text,       \
       SizeX        Integer,    \
       SizeY        Integer );" ;

  int e = sqlite3_exec(db, init, NULL, NULL, NULL);
  if(e != SQLITE_OK) {
    sqlite3_close(db);
    error = 1;
    return;
  }
}

void i_add_note(const char* text, const char* tag, int size_x, int size_y) {
//  // add the note to 
//  //   (1) Note
//  //   (2) View.ViewId "YYYYMMDD" if such a view exists
//  //   (3) View.ViewID tag        if tag != ""
// 
//  // (1)
//  //  get a new NoteId
//  int note_id = 0;
//  std::function<int(int,int&)> callback = 
//    [&note_id](int v, int&){ 
//      note_id = v + 1;
//      return 0;
//    };
//  int e = exec(db,
//    "SELECT MAX(NoteId) FROM Note;", 
//    note_id,
//    callback);
//
//  if (e != SQLITE_OK) { 
//    sqlite3_close(db);
//    error = e;
//    return;
//  }
//
//  const char* today = "YYYYMMDD";
//
//  char* sql = sqlite3_mprintf(
//      "INSERT INTO Note ( NoteId, Text, DateCreated, DateChanged, SizeX, SizeY ) "\
//      "VALUES( %d, \"%w\", \"%w\", \"%w\", %d, %d );", 
//      note_id, text, today, today, size_x, size_y);
//  std::cout << sql << std::endl;
//
//  e = sqlite3_exec(db, sql, NULL, NULL, NULL);
//  if (e != SQLITE_OK) { 
//    sqlite3_close(db);
//    error = e;
//    return;
//  }
//
//  std::cout << "NOTE ID " << note_id << std::endl;
//  sqlite3_free(sql);
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

