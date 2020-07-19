#ifndef COM_JSON_H
#define COM_JSON_H

#include "com_bigint.h"
#include "com_define.h"
#include "com_streamposition.h"
#include "com_writer.h"
#include "com_reader.h"

typedef enum {
  com_json_INVALID = 0,
  com_json_NULL,
  com_json_BOOL,
  com_json_INT,
  com_json_NUM,
  com_json_STR,
  com_json_ARRAY,
  com_json_OBJ,
} com_json_ElemKind;

typedef struct com_json_Elem_s com_json_Elem;
typedef struct com_json_Prop_s com_json_Prop;

typedef struct com_json_Elem_s {
  com_json_ElemKind kind;
  union {
    bool boolean;
    double number;
    i64 integer;
    com_str string;
    struct {
      com_json_Elem *values;
      usize length;
    } array;
    struct {
      com_json_Prop *props;
      usize length;
    } object;
  };
} com_json_Elem;

typedef struct com_json_Prop_s {
  com_str key;
  com_json_Elem value;
} com_json_Prop;

#define com_json_prop_m(k, v) ((com_json_Prop){.key = (k), .value = (v)})
#define com_json_null_m ((com_json_Elem){.kind = com_json_NULL})
#define com_json_invalid_m ((com_json_Elem){.kind = com_json_NULL})
#define com_json_bool_m(v)                                                     \
  ((com_json_Elem){.kind = com_json_BOOL, .boolean = (v)})
#define com_json_int_m(v)                                                      \
  ((com_json_Elem){.kind = com_json_INT, .integer = (v)})
#define com_json_num_m(v) ((com_json_Elem){.kind = com_json_NUM, .number = (v)})
#define com_json_str_m(v) ((com_json_Elem){.kind = com_json_STR, .string = (v)})
#define com_json_array_m(v, len)                                               \
  ((com_json_Elem){.kind = com_json_ARRAY, .array = {.values = (v), .length = (len)}})
#define com_json_obj_m(v, len)                                                 \
  ((com_json_Elem){.kind = com_json_OBJ, .object = {.props = (v), .length = (len)}})

#define com_json_obj_lit_m(arr) com_json_obj_m((arr), sizeof(arr))
#define com_json_arr_lit_m(arr) com_json_array_m((arr), sizeof(arr))

// Parse JSON
// JSON Parsing errors
typedef enum {
  com_json_ElemEof,
  com_json_ElemUnknownCharacter,
  com_json_MalformedLiteral,
  com_json_StrExpectedDoubleQuote,
  com_json_StrInvalidControlChar,
  com_json_StrInvalidUnicodeSpecifier,
  com_json_NumExponentExpectedSign,
  com_json_ArrayExpectedRightBracket,
  com_json_ArrayExpectedJsonElem,
  com_json_ObjectExpectedRightBrace,
  com_json_ObjectExpectedProp,
  com_json_PropExpectedDoubleQuote,
  com_json_PropExpectedColon,
  com_json_PropExpectedValue,
} com_json_ErrorKind;

typedef struct {
  com_json_ErrorKind kind;
  com_streamposition_LnCol loc;
} com_json_Error;

#define com_json_error_m(k, l) ((com_json_Error){.kind = k, .loc = l})

// serializes json to a writer (100% static no allocator needed)
void com_json_serialize(com_json_Elem* elem, com_writer* writer);

// converts an inputstream into a json DOM
com_json_Elem com_json_parseElem(com_reader* reader, com_vec* diagnostics,  com_Allocator* allocator);

#endif

