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

void add_view_note(const char* view_id, int note_id, int l, int r, int u, int d) {
  schar_ptr_t add_it_sql(sqlite3_mprintf(
    "INSERT INTO ViewNote ( ViewId, NoteId, LocL, LocR, LocU, LocD ) "\
    "VALUES( \"%w\", %d, %d, %d, %d, %d );", view_id, note_id, l, r, u, d));
  i_check_(sqlite3_exec(db, add_it_sql.get(), NULL, NULL, NULL));
}

void add_note(int note_id, const char* text, const char* create, const char* change) {
  schar_ptr_t add_it_sql(sqlite3_mprintf(
    "INSERT INTO Note ( NoteId, Text, DateCreated, DateChanged ) "\
    "VALUES( %d, \"%w\", \"%w\", \"%w\" );", note_id, text, create, change));
  i_check_(sqlite3_exec(db, add_it_sql.get(), NULL, NULL, NULL));
}

int count_it(const char* sql) {
  std::function<int(integer_t,int&)> count_callback = 
    [](integer_t v, int& count_){ count_ = v; return 0; };
  int count = 0;
  i_check(exec(db, sql, count, count_callback),count);
  return count;
}

bool has_view(const char* view_id) {
  schar_ptr_t sql(sqlite3_mprintf(
    "SELECT COUNT(*) FROM View WHERE ViewId == \"%w\";", 
    view_id));
  return count_it(sql.get()) > 0;
}

// check if bounding box of l/r/u/d intersects with 
// any note portion (i.e. l/r/u/d can touch boundaries but not text)
// in the view_Id
bool area_has_note(int l, int r, int u, int d, const char* view_id) {
  // max(LocL+1,l) <= min(LocR-1,r) and 
  // max(LocU+1,u) <= min(LocD-1,d)
  schar_ptr_t sql(sqlite3_mprintf(
    "SELECT COUNT(*) FROM ViewNote WHERE ViewId == \"%w\" "
    "and max(LocL+1,%d) <= min(LocR-1,%d) "
    "and max(LocU+1,%d) <= min(LocD-1,%d);",
    view_id, l, r, u, d));
  return count_it(sql.get()) > 0;
}

int max_note_id() {
  return count_it("SELECT MAX(NoteId) FROM Note;");
}

////////////////////////////////////////////////////////////////////////////////

void i_exec(const char* sql) {
  error = sqlite3_exec(db, sql, NULL, NULL, NULL);
}

sqlite3_stmt** stmt_init(const char* sql) {
  sqlite3_stmt** ppStmt = new (sqlite3_stmt*);
  error = sqlite3_prepare(db, sql, -1, ppStmt, NULL);
  return ppStmt;
}

void stmt_finalize(sqlite3_stmt** ppStmt) {
  error = sqlite3_finalize(*ppStmt);
  delete ppStmt;
}

bool stmt_increment(sqlite3_stmt** ppStmt) {
  int e = sqlite3_step(*ppStmt);
  if(e == SQLITE_DONE) {
    return true;
  } 
  if(e == SQLITE_ROW) {
    return false;
  }
  error = e;
  return true;
}

integer_t stmt_column_int(sqlite3_stmt** ppStmt, int column) {
  return sqlite3_column_int(*ppStmt, column);
}
float_t stmt_column_double(sqlite3_stmt** ppStmt, int column) {
  return sqlite3_column_double(*ppStmt, column);
}
blob_t stmt_column_blob(sqlite3_stmt** ppStmt, int column) {
  return sqlite3_column_blob(*ppStmt, column);
}
text_t stmt_column_text(sqlite3_stmt** ppStmt, int column) {
  return sqlite3_column_text(*ppStmt, column);
}




