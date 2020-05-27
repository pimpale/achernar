#ifndef JSON_H_
#define JSON_H_

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

#include "lncol.h"
#include "lexer.h"

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
    struct {
      char *string;
      size_t length;
    } string;
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
  char *key;
  JsonElem value;
} JsonKV;

// Utility functions
JsonKV KVJson(char *key, JsonElem value);
JsonElem nullJson(void);
JsonElem boolJson(bool x);
JsonElem intJson(uint64_t x);
JsonElem numJson(double x);
JsonElem strJson(char *x, size_t len);
JsonElem arrDefJson(JsonElem *ptr, size_t len);
JsonElem objDefJson(JsonKV *ptr, size_t len);

char *toStringJsonElem(JsonElem *j);

// Parse JSON
// JSON Parsing errors
typedef enum JsonParseDiagnosticKind_e {
  JPDK_JsonStringExpectedDoubleQuote,
  JPDK_JsonStringInvalidControlChar,
  JPDK_JsonStringInvalidUnicodeSpecifier,
  JPDK_JsonMalformedLiteral,
  JPDK_JsonNumberExponentExpectedSign,
  JPDK_JsonArrayExpectedRightBracket,
  JPDK_JsonArrayExpectedJsonElem,
  JPDK_JsonObjExpectedRightBrace,
  JPDK_JsonObjExpectedJsonKV,
} JsonParseDiagnosticKind ;

typedef struct JsonParseDiagnostic_s {
  JsonParseDiagnosticKind kind;
  LnCol loc;
} JsonParseDiagnostic;

#define JSONPARSEDIAGNOSTIC(k, l) ((JsonParseDiagnostic) { .kind=k, .loc=l})

void parseJsonElem(JsonElem *je, Lexer *l, Vector* diagnostics);

#endif
