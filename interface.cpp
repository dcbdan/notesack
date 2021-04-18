#include "interface.h"
#include <stddef.h> // NULL
#include <string>

// REMOVE THESE
#include <iostream>

sqlite3* db;
int error = 0;

#define i_check(e,v)       \
  error = e;               \
  if(error != SQLITE_OK) { \
    sqlite3_close(db);     \
    return v;              \
  }
#define i_check_(e)        \
  error = e;               \
  if(error != SQLITE_OK) { \
    sqlite3_close(db);     \
    return;                \
  }

void i_open(const char* filename) {
  i_check_(sqlite3_open(filename, &db))
  i_check_(!db)
}

void i_init() {
  const char* init = 
    "CREATE TABLE View (        \
       ViewId       Text         PRIMARY KEY,\
       LocX         Integer,    \
       LocY         Integer,    \
       Selected     Integer );  \
     CREATE TABLE ViewNote (    \
       ViewId       Text,       \
       NoteId       Integer,    \
       LocL         Integer,    \
       LocR         Integer,    \
       LocU         Integer,    \
       LocD         Integer,    \
                                 PRIMARY KEY( ViewId, NoteId )\
     );                         \
     CREATE TABLE Note (        \
       NoteId       Integer      PRIMARY KEY,\
       Text         Text,       \
       DateCreated  Text,       \
       DateChanged  Text );"    ;

  i_check_(sqlite3_exec(db, init, NULL, NULL, NULL))
}

// close the database then close the so
void i_close() {
  error = sqlite3_close(db);
}

int i_error() {
  return error;
}

void add_view_no_selected(const char* view_id, int loc_x, int loc_y) {
  schar_ptr_t add_it_sql(sqlite3_mprintf(
    "INSERT INTO View ( ViewId, LocX, LocY ) "\
    "VALUES( \"%w\", %d, %d );", view_id, loc_x, loc_y));
  i_check_(sqlite3_exec(db, add_it_sql.get(), NULL, NULL, NULL));
}

void add_view(const char* view_id, int loc_x, int loc_y, int note_id) {
  schar_ptr_t add_it_sql(sqlite3_mprintf(
    "INSERT INTO View ( ViewId, LocX, LocY, Selected ) "\
    "VALUES( \"%w\", %d, %d, %d );", view_id, loc_x, loc_y, note_id));
  i_check_(sqlite3_exec(db, add_it_sql.get(), NULL, NULL, NULL));
}

int count_it(schar_ptr_t& sql) {
  std::function<int(text_t,int&)> count_callback = 
    [](text_t, int& count_){ count_ += 1; return 0; };
  int count = 0;
  i_check(exec(db, sql.get(), count, count_callback),count);
  return count;
}

bool has_view(const char* view_id) {
  schar_ptr_t sql(sqlite3_mprintf(
    "SELECT SUM(1) FROM View WHERE ViewId == \"%w\";", 
    view_id));
  return count_it(sql);
}

bool area_has_note(int l, int r, int u, int d, const char* view_id) {
  // max(LocL+1,l) <= min(LocR-1,r) and 
  // max(LocU+1,u) <= min(LocD-1,d)
  schar_ptr_t sql(sqlite3_mprintf(
    "SELECT SUM(1) FROM View WHERE ViewId == \"%w\" "
    "and max(LocL+1,%d) <= min(LocR-1,%d) "
    "and max(LocU+1,%d) <= min(LocD-1,%d);",
    view_id, l, r, u, d));
  return count_it(sql);
}


