#include "interface.h"
#include <dlfcn.h>
#include <stddef.h>
#include <stdio.h>

int error = 0;
void* soLib = NULL;
sqlite3* db;

typedef int(*openT)(const char*,sqlite3**);
typedef int(*closeT)(sqlite3*);
openT  openFunc  = NULL;
closeT closeFunc = NULL;

// open the so           ->  if success ->
// load in the functions ->  if success ->
// load the database
// (if no success, close the solib)
void i_open(const char* filename) {
  error = 0;

  dlerror();
  soLib = dlopen(sqlite3so, RTLD_NOW | RTLD_DEEPBIND);
  char* err = dlerror();
  if(!soLib || err) {
    error = 1;
    return;
  }
 
  void* open_ = dlsym(soLib, "sqlite3_open");
  if(!open_) {
    dlclose(soLib);
    error = 1;
    return;
  }
  openFunc = (openT)open_;
  
  void* close_ = dlsym(soLib, "sqlite3_close");
  if(!close_) {
    dlclose(soLib);
    error = 1;
    return;
  }
  closeFunc = (closeT)close_;

  int e = openFunc(filename, &db);
  if(e != SQLITE_OK) {
    closeFunc(db);
    dlclose(soLib);
    error = 1;
    return;
  }
  if(!db){
    dlclose(soLib);
    error = 1;
    return;
  }
}

// close the database then close the so
void i_close() {
  error = 0;

  int e = closeFunc(db);
  if(e != SQLITE_OK) {
    error = 1;
  }

  dlerror();
  dlclose(soLib);
  char* err = dlerror();
  if(err) {
    error = 1;
  }
}

int i_err() {
  return error;
}

