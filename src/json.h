#ifndef JSON_H_
#define JSON_H_

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

typedef enum {
  JE_boolean,
  JE_integer,
  JE_number,
  JE_string,
  JE_array,
  JE_object,
  JE_null,
} JsonElemKind;

typedef struct JsonElem_s JsonElem;
typedef struct JsonKV_s JsonKV;

typedef struct JsonElem_s {
  JsonElemKind kind;
  union {
    bool boolean;
    uint64_t integer;
    double number;
    char* string;
    struct {
      JsonElem *values;
      size_t length;
    } array;
    struct {
      JsonKV *values;
      size_t length;
    } object;
  };
} JsonElem;

typedef struct JsonKV_s {
  char* key;
  JsonElem value;
} JsonKV;


#define JKV(k, v) ((JsonKV){.key=(k), .value=(v)})
#define J_NULL ((JsonElem){.kind=JE_null})
#define J_BOOL(x) ((JsonElem){.kind=JE_boolean, .boolean=(x)})
#define J_INT(x) ((JsonElem){.kind=JE_integer, .integer=(x)})
#define J_NUM(x) ((JsonElem){.kind=JE_number, .number=(x)})
#define J_STR(x) ((JsonElem){.kind=JE_string, .string=(x)})
#define J_ARR_DEF(ptr, len) ((JsonElem){.kind=JE_array, .array={.values=(ptr), .length=(len)}})
#define J_OBJ_DEF(ptr, len) ((JsonElem){.kind=JE_object, .object={.values=(ptr), .length=(len)}})

char* toStringJsonElem(JsonElem* j);

#endif
