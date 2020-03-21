#include "json.h"
#include "arena.h"
#include "vector.h"

#include <inttypes.h>
#include <stdio.h>
#include <string.h>

static void pushCharJson(Vector *vptr, char c) {
  *VEC_PUSH(vptr, char) = c;
}

// Checks for special characters
static void escapePushStrJson(Vector *vptr, char *str) {
  size_t i = 0;
  while (str[i] != 0) {
    char c = str[i];
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
      ptr[1] = '\"';
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
    i++;
  }
}

// Does not check for special characters (optimised)
// Pushes string Exlcuding null byte
static void pushStrJson(Vector *vptr, char *str) {
  // length of string in bytes
  size_t len = strlen(str) * sizeof(char);
  memcpy(pushVector(vptr, len), str, len);
}

static void toStringJsonElemRec(JsonElem *j, Vector *data) {
  switch (j->kind) {
  case JE_null: {
    pushStrJson(data, "null");
    break;
  }
  case JE_string: {
    pushCharJson(data, '\"');
    escapePushStrJson(data, j->string);
    pushCharJson(data, '\"');
    break;
  }
  case JE_boolean: {
    pushStrJson(data, j->boolean ? "true" : "false");
    break;
  }
  case JE_integer: {
    // there are a max 20 digits in an integer
    char str[20];
    snprintf(str, 20, "%" PRIu64, j->integer);
    pushStrJson(data, str);
    break;
  }
  case JE_number: {
    // up to 328 digits in a float
    char str[350];
    snprintf(str, 350, "%f", j->number);
    pushStrJson(data, str);
    break;
  }
  case JE_array: {
    pushCharJson(data, '[');
    for (size_t i = 0; i < j->array.length; i++) {
      toStringJsonElemRec(&j->array.values[i], data);
      // if we are not on the ultimate element of the lsit
      if (i + 1 != j->array.length) {
        pushCharJson(data, ',');
      }
    }
    pushCharJson(data, ']');
    break;
  }
  case JE_object: {
    pushCharJson(data, '{');
    for (size_t i = 0; i < j->object.length; i++) {
      JsonKV jkv = j->object.values[i];
      pushCharJson(data, '\"');
      escapePushStrJson(data, jkv.key);
      pushStrJson(data, "\":");
      toStringJsonElemRec(&jkv.value, data);
      if (i + 1 != j->object.length) {
        pushCharJson(data, ',');
      }
    }

    pushCharJson(data, '}');
    break;
  }
  }
}

char *toStringJsonElem(JsonElem *j) {
  Vector data;
  createVector(&data);
  toStringJsonElemRec(j, &data);
  // terminate string
  pushCharJson(&data, '\0');
  return releaseVector(&data);
}
