#ifndef JSON_H_
#define JSON_H_

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <string.h>

#include "lexer.h"
#include "utils.h"
#include "lncol.h"
#include "vector.h"

typedef enum {
  j_NullKind = 0,
  j_BoolKind,
  j_IntKind,
  j_NumKind,
  j_StrKind,
  j_ArrayKind,
  j_ObjectKind,
} j_ElemKind;

typedef struct j_Elem_s j_Elem;
typedef struct j_Prop_s j_Prop;

typedef struct j_Int_s {
  bool negative;
  uint64_t integer;
} j_Int;

typedef struct j_Str_s {
  const char *string;
  size_t length;
} j_Str;

typedef struct j_Elem_s {
  j_ElemKind kind;
  union {
    bool boolean;
    double number;
    j_Int integer;
    j_Str string;
    struct {
      j_Elem *values;
      size_t length;
    } array;
    struct {
      j_Prop *props;
      size_t length;
    } object;
  };
} j_Elem;

typedef struct j_Prop_s {
  j_Str key;
  j_Elem value;
} j_Prop;

// Utility macros to construct these types
#define J_ASCIZ(x) J_STR((x), strlen((x)))
// string literals only
#define J_LITSTR(x) J_STR((x), sizeof(x)-1)
#define J_STR(x, len) ((j_Str){.string = (x), .length = (len)})


#define J_INT(neg, val) ((j_Int){.negative=(neg), .integer=(val)})
#define J_UINT(x) J_INT(false, (x))
#define J_SINT(x) J_INT(((x) < 0), safe_abs((x)))

#define J_PROP(k, v) ((j_Prop){.key = (k), .value = (v)})

#define J_NULL_ELEM ((j_Elem){.kind = j_NullKind})
#define J_BOOL_ELEM(v) ((j_Elem){.kind = j_BoolKind, .boolean = (v)})
#define J_INT_ELEM(v) ((j_Elem){.kind = j_IntKind, .integer = (v)})
#define J_STR_ELEM(v) ((j_Elem){.kind = j_StrKind, .string = (v)})
#define J_NUM_ELEM(v) ((j_Elem){.kind = j_NumKind, .number = (v)})
#define J_ARRAY_ELEM(v, len)                                                   \
  ((j_Elem){.kind = j_ArrayKind, .array = {.values = (v), .length = (len)}})
#define J_OBJECT_ELEM(v, len)                                                  \
  ((j_Elem){.kind = j_ObjectKind, .object = {.props = (v), .length = (len)}})

char *j_stringify(j_Elem *j, Allocator *a);

// Parse JSON
// JSON Parsing errors
typedef enum {
  j_ElemEof,
  j_ElemUnknownCharacter,
  j_MalformedLiteral,
  j_StrExpectedDoubleQuote,
  j_StrInvalidControlChar,
  j_StrInvalidUnicodeSpecifier,
  j_NumExponentExpectedSign,
  j_ArrayExpectedRightBracket,
  j_ArrayExpectedJsonElem,
  j_ObjectExpectedRightBrace,
  j_ObjectExpectedJsonKV,
  j_PropExpectedColon,
  j_PropExpectedValue,
} j_ErrorKind;

typedef struct j_Error_s {
  j_ErrorKind kind;
  LnCol loc;
} j_Error;

// Json DOM
j_Elem j_parseElem(Lexer *l, Vector *diagnostics, Allocator *a);

#endif
