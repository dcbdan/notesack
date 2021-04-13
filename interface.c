#include "interface.h"
#include <dlfcn.h>
#include <stddef.h>
#include <stdio.h>

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
}

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

