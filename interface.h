#include "sqlite3.h"
#include <tuple>
#include <functional>
#include <memory>
#include <type_traits>

extern "C" {

// when these were named open / close / err, close would get
// called when compiling with
//  ghc -o notesack -threaded -outputdir build Main.hs interface.c
// even when main = return ()
// .. It didn't happen when the threaded option was removed
  void i_open(const char* filename);
  void i_init();
  void i_close();
  int i_error();
  void add_view_no_selected(const char* view_id, int loc_x, int loc_y);
  void add_view(const char* view_id, int loc_x, int loc_y, int selected);
  bool has_view(const char* view_id);
}

///////////////////////////////////////////////////////////////////////////////

using integer_t = int;
using float_t   = double;
using blob_t    = const void*;
using text_t    = const unsigned char*;
struct null_t {};
// ^ if you don't actually want a column, use null_t

template <typename T>
using enable_integer_t = std::enable_if_t<std::is_same<T, integer_t>::value, T >;

template <typename T>
using enable_float_t   = std::enable_if_t<std::is_same<T, float_t  >::value, T >;

template <typename T>
using enable_blob_t    = std::enable_if_t<std::is_same<T, blob_t   >::value, T >;

template <typename T>
using enable_text_t    = std::enable_if_t<std::is_same<T, text_t   >::value, T >;

template <typename T>
using enable_null_t    = std::enable_if_t<std::is_same<T, null_t   >::value, T >;

template<typename T>
struct is_tuple : std::false_type {};

template<typename... Ts>
struct is_tuple<std::tuple<Ts...>> : std::true_type {};

template <typename T>
using enable_tuple_t = std::enable_if_t<is_tuple<T>::value, T>;

template <typename T>
using enable_not_tuple_t = std::enable_if_t<!is_tuple<T>::value, T>;

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

///////////////////////////////////////////////////////////////////////////////

struct schar_delete_t {
  void operator()(char* ptr){
    sqlite3_free(ptr);
  }
};

using schar_ptr_t = std::unique_ptr<char, schar_delete_t>;


