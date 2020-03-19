#include "json.h"
#include "vector.h"

#include <string.h>

static void pushCharJson(Vector *vptr, char c) {
  switch (c) {
  case '\b': {
    char *ptr = pushVector(vptr, sizeof(char) * 2);
    ptr[0] = '\\';
    ptr[1] = 'b';
    break;
  }
  case '\f': {
    char *ptr = pushVector(vptr, sizeof(char) * 2);
    ptr[0] = '\\';
    ptr[1] = 'f';
    break;
  }
  case '\n': {
    char *ptr = pushVector(vptr, sizeof(char) * 2);
    ptr[0] = '\\';
    ptr[1] = 'n';
    break;
  }
  case '\r': {
    char *ptr = pushVector(vptr, sizeof(char) * 2);
    ptr[0] = '\\';
    ptr[1] = 'r';
    break;
  }
  case '\t': {
    char *ptr = pushVector(vptr, sizeof(char) * 2);
    ptr[0] = '\\';
    ptr[1] = 't';
    break;
  }
  case '\"': {
    char *ptr = pushVector(vptr, sizeof(char) * 2);
    ptr[0] = '\\';
    ptr[1] = '\\';
    break;
  }
  case '\\': {
    char *ptr = pushVector(vptr, sizeof(char) * 2);
    ptr[0] = '\\';
    ptr[1] = '\\';
    break;
  }
  default: {
    *VEC_PUSH(vptr, char) = c;
  }
  }
}

static void pushStrJson(Vector *vptr, char *str) {
  size_t i = 0;
  while (true) {
    pushCharJson(vptr, str[i]);
    if (str[i] == 0) {
      break;
    }
  }
}

static void toStringJsonElemRec(JsonElem *j, Vector *data) {
  switch (j->kind) {
  case JE_null: {
    pushStrJson(data, "null");
    break;
  }
  case JE_string: {
    pushCharJson(data, '\"');
    pushStrJson(data, j->string);
    pushCharJson(data, '\"');
    break;
  }
  case JE_boolean: {
    pushStrJson(data, j->boolean ? "true" : "false");
    break;
  }
  case JE_integer:

      size_t len = strlen(
      memcpy(pushVector(data, 5),
  }
  }

  char *toStringJsonElem(JsonElem * j) {
    Vector data;
    createVector(&data);
    toStringJsonElemRec(j, &data);
    return releaseVector(&data);
  }
