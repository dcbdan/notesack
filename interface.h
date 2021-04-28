#include "sqlite3.h"
#include <tuple>
#include <functional>
#include <memory>
#include <type_traits>
#include <vector>

using integer_t = int;
using float_t   = double;
using blob_t    = const void*;
using text_t    = const unsigned char*;
struct null_t {};
// ^ if you don't actually want a column, use null_t

extern "C" {
  void i_init();
  void add_view_no_selected(const char* view_id, int loc_x, int loc_y);
  void add_view(const char* view_id, int loc_x, int loc_y, int selected);
  void add_view_note(const char* view_id, int note_id, int l, int r, int u, int d);
  void add_note(int note_id, const char* text, const char* create, const char* change);
  bool has_view(const char* view_id);
  bool area_has_note(int,int,int,int,const char*);
  int max_note_id();
  ////////////////////////////////////////////
  // when these were named open / close / err, close would get
  // called when compiling with
  //  ghc -o notesack -threaded -outputdir build Main.hs interface.c
  // even when main = return ()
  // .. It didn't happen when the threaded option was removed
  // TODO: phase out the api functions above for just below
  void i_open(const char* filename);
  void i_close();
  int i_error();
  void i_exec(const char* sql);
  sqlite3_stmt** stmt_init(const char* sql);
  void stmt_finalize(sqlite3_stmt** stmt);
  bool stmt_increment(sqlite3_stmt** stmt);
  integer_t stmt_column_int   (sqlite3_stmt** ppStmt, int column);
  float_t   stmt_column_double(sqlite3_stmt** ppStmt, int column);
  blob_t    stmt_column_blob  (sqlite3_stmt** ppStmt, int column);
  text_t    stmt_column_text  (sqlite3_stmt** ppStmt, int column);
  char* mprintf1(const char* sql, const char* arg);
  void free_mprintf(char* ptr);
}

///////////////////////////////////////////////////////////////////////////////


template <typename T, typename U, typename Out>
using enable_if_same_out = std::enable_if_t<std::is_same<T, U>::value, Out >;

template <typename T, typename U>
using enable_if_same = enable_if_same_out<T,U,T>;

template <typename T>
using enable_integer_t = enable_if_same<T, integer_t>;

template <typename T>
using enable_float_t   = enable_if_same<T, float_t  >;

template <typename T>
using enable_blob_t    = enable_if_same<T, blob_t   >;

template <typename T>
using enable_text_t    = enable_if_same<T, text_t   >;

template <typename T>
using enable_null_t    = enable_if_same<T, null_t   >;

template<typename T>
struct is_tuple : std::false_type {};

template<typename... Ts>
struct is_tuple<std::tuple<Ts...>> : std::true_type {};

template <typename T, typename U=T>
using enable_tuple_t = std::enable_if_t<is_tuple<T>::value, U>;

template <typename T, typename U=T>
using enable_not_tuple_t = std::enable_if_t<!is_tuple<T>::value, U>;

template <class T, int N>
enable_integer_t<T>
access_helper(sqlite3_stmt* stmt) {
  return sqlite3_column_int(stmt, N);
}

template <class T, int N>
enable_float_t<T>
access_helper(sqlite3_stmt* stmt) {
  return sqlite3_column_double(stmt, N);
}

template <class T, int N>
enable_blob_t<T>
access_helper(sqlite3_stmt* stmt) {
  return sqlite3_column_blob(stmt, N);
}

template <class T, int N>
enable_text_t<T>
access_helper(sqlite3_stmt* stmt) {
  return sqlite3_column_text(stmt, N);
}

template <class T, int N>
enable_null_t<T>
access_helper(sqlite3_stmt*) {
  return null_t{};
}

template <typename T, int N>
std::tuple<T> access_tuple(sqlite3_stmt* stmt) {
  return std::make_tuple(access_helper<T,N>(stmt));
}

template <typename T, typename... Ts, int N>
std::tuple<T, Ts...> access_tuple(sqlite3_stmt* stmt) {
  return std::tuple_cat(
      std::make_tuple(access_helper<T,N>(stmt)),
      access_tuple<Ts..., N+1>(stmt));
}

template <typename T>
enable_not_tuple_t<T>
access(sqlite3_stmt* stmt) {
  return access_helper<T,0>(stmt); 
}

template <typename T>
enable_tuple_t<T>
access(sqlite3_stmt* stmt) {
  return access_tuple<T,0>(stmt);
}

// This function serves the same purpose
// as sqlite3_exec except
// (1) sqlite3_exec converts
//     each element to text, whereas this function
//     converts each row into either
//     a single value for single columns or 
//     a tuple of values.
// (2) this function has less error handling,
//
// U can be integer_t, float_t, blob_t, text_t
// or a a tuple thereof.
template <typename T, typename U>
int exec(
    sqlite3* db, 
    const char* sql, 
    T& v, 
    std::function<int(U,T&)> callback)
{
  sqlite3_stmt *stmt;

  // the docs say to use _v2 since prepare to get 
  // more detailed error codes, but I don't need
  // that for now
  sqlite3_prepare(db, sql, -1, &stmt, NULL);
  
  while(true) {
    int e = sqlite3_step(stmt);
    if(e == SQLITE_DONE) {
      break;
    }
    else if(e == SQLITE_ROW) {
      U u = access<U>(stmt);
      int maybeErr = callback(access<U>(stmt), v);
      if(maybeErr) {
        break;
      }
    } else {
      break;
    }
  }

  return sqlite3_finalize(stmt);
}

template <typename U>
int exec_to_vector(
    sqlite3* db,
    const char* sql,
    std::vector<U>& ret)
{
  std::function<int(U,std::vector<U>&)> callback = [](U u, std::vector<U>& v) {
    v.push_back(u);
    return 0;
  };
  return exec(db, sql, ret, callback);
}

///////////////////////////////////////////////////////////////////////////////

struct schar_delete_t {
  void operator()(char* ptr){
    sqlite3_free(ptr);
  }
};

using schar_ptr_t = std::unique_ptr<char, schar_delete_t>;


