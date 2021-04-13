#include "interface.h"
#include <stddef.h> // NULL

int error = 0;
sqlite3* db;

void i_open(const char* filename) {
  error = 0;

  int e = sqlite3_open(filename, &db);
  if(e != SQLITE_OK) {
    sqlite3_close(db);
    error = 1;
    return;
  }

  if(!db){
    error = 1;
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

//void i_callback_get_row_id(void* v, int num_rows, char** table, char**) {
//  int& ret = *v;
//  if(num_rows == 0) {
//    v = 0; 
//  }
//
//}
//int i_new_row_id() {
//  sqlite3_stmt *stmt;
//  sqlite3_prepare_v2(db,
//    "SELECT last_insert_rowid() FROM Note LIMIT 1;",
//    -1,
//    &stmt,
//    NULL);
//  int e = sqlite3_step(stmt);
//  if(e == SQLITE_ROW) {
//
//  }
//  int ret = sqlite3_column_int(stmt, 0);
//  
//}
//
//void i_add_note(const char* text, const char* tag, int size_x, int size_y) {
//  // add the note to 
//  //   (1) Note
//  //   (2) View.ViewId "YYYYMMDD" if such a view exists
//  //   (3) View.ViewID tag        if tag != ""
// 
//  // (1)
//  //  get a new NoteId
//  int note_id = 0;
//  int e = sqlite3_exec(db,
//    "SELECT last_insert_rowid() FROM Note LIMIT 1;", 
//    &i_callback_get_row_id, 
//    &note_id,
//    NULL);
//  if (e != SQLITE_OK) { 
//    sqlite3_close(db);
//    error = 1;
//  }
//}

// close the database then close the so
void i_close() {
  error = 0;

  int e = sqlite3_close(db);
  if(e != SQLITE_OK) {
    error = 1;
  }
}

int i_err() {
  return error;
}

